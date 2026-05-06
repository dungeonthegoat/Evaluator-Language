grammar evaluator;


abstract production letRecExpr
e::Expr ::= name::String param::String paramType::Type returnType::Type body::Expr e2::Expr
{
  propagate typeErrors;

  local selfType :: Type = funcType(^paramType, ^returnType);

  e2.typeEnv = (name, ^selfType) :: e.typeEnv;
  e2.env = (name, recClosureVal(name, param, ^body, e.env)) :: e.env;

  body.typeEnv = (name, ^selfType) :: (param, ^paramType) :: e.typeEnv;
  body.env = e.env;

  e.type = e2.type;
  e.value = e2.value;

  local retTypeError :: [String] = case body.type, returnType of
  | t1, t2 when equalsType(t1, ^t2) -> []
  | _, errType() -> []
  | errType(), _ -> []
  | _, _ -> ["Return type of function '" ++ name ++ "' does not match its declared return type"]
  end;

  e.typeErrors <- retTypeError;
}

abstract production lambda
e::Expr ::= param::String paramType::Type b::Expr
{
  propagate env, typeErrors;
  
  b.typeEnv = (param, ^paramType) :: e.typeEnv ;

  e.type = funcType(^paramType, b.type);
  e.value = closureVal(param, ^b, e.env);
}

abstract production app
e::Expr ::= func::Expr arg::Expr
{
  propagate env, typeEnv, typeErrors;

  local paramType :: Type = case func.type of
  | funcType(p, _) -> ^p
  | _ -> errType()
  end;

  local returnType :: Type = case func.type of
  | funcType(_, r) -> ^r
  | _ -> errType()
  end;

  local funcTypeError :: [String] = case func.type of
  | funcType(_, _) -> []
  | errType() -> []
  | _ -> ["Attempted application of a non-function"]
  end;

  local argMatchParamError :: [String] =
    if equalsType(arg.type, ^paramType) || (equalsType(arg.type, errType()))
    then []
    else [
          "Argument type " ++
          toStringType(arg.type) ++
          " does not match expected parameter type " ++
          toStringType(^paramType)
        ];
  
  local param :: String = case func.value of
  | closureVal(p, _, _) -> p
  | recClosureVal(_, p, _, _) -> p
  | _ -> error("Type error: expected a function")
  end;

  local body :: Expr = case func.value of
  | closureVal(_, b, _) -> ^b
  | recClosureVal(_, _, b, _) -> ^b
  | _ -> error("Type error: expected a function")
  end;

  local f_env :: [Pair<String Value>] = case func.value of
  | closureVal(_, _, f) -> f
  | recClosureVal(name, _, _, f) -> pair(fst=name, snd=func.value) :: f
  | _ -> []
  end;

  body.env = pair(fst=param, snd=arg.value) :: f_env;

  e.type = ^returnType;
  e.value = body.value;

  e.typeErrors <- funcTypeError ++ argMatchParamError;
}

abstract production typeDeclaration
e::Expr ::= name::String constructors::[(String, [Type])] rest::Expr
{
  propagate typeErrors;

  rest.env = constructConstructorEnv(constructors, name, e.env) ++ e.env;
  rest.typeEnv = constructConstructorTypeEnv(constructors, name) ++ e.typeEnv;

  e.type = rest.type;
  e.value = rest.value;

  -- Questionable formatting
  e.pp = "type " ++ name ++ " = " ++ 
    implode(" | ",
      map(\constr::(String, [Type]) ->
        fst(constr) ++ " of " ++ "(" ++ implode(", ", map(toStringType, snd(constr))) ++ ")"
      , constructors)
    ) ++ " in\n" ++ rest.pp;
}

-- Concrete syntax

concrete production lambda_c
e::Expr_c ::= Lambda param::Var Colon t::Type_c Arrow body::Expr_c
{
  e.ast = lambda(param.lexeme, t.type_ast, body.ast);
}

concrete production func
e::Expr_c ::= Let retT::Type_c name::Var params::TypedVarList_c Eq body::Expr_c In rest::Expr_c
{
  e.ast = letRecExpr(
    name.lexeme, 
    head(params.vars).fst,
    head(params.vars).snd,
    constructFuncType(tail(params.vars), retT.type_ast),
    constructLambdas(tail(params.vars), body.ast), 
    rest.ast
  );
}

nonterminal App_c with ast;

concrete productions a::App_c
| t::Term_c { a.ast = t.ast; }
| f::App_c t::Term_c { a.ast = app(f.ast, t.ast); }

-- Functions

@{- Returns a curried lambda expression representing a function of multiple parameters -}
function constructLambdas
Expr ::= params::[(String, Type)] body::Expr
{
  return case params of
  | [] -> ^body
  | (n, t)::ps -> lambda(n, t, constructLambdas(ps, ^body))
  end;
}

@{- Returns the type of a multi-parameter function with a defined return type by currying funcTypes-}
function constructFuncType
Type ::= params::[(String, Type)] retType::Type
{
  return case params of
  | [] -> ^retType
  | (_, t)::ps -> funcType(t, constructFuncType(ps, ^retType))
  end;
}

@{- Turns each constructor into a funcType representing its constructor function's type, and returns the list of such constructor types -}
function constructConstructorTypeEnv
[(String, Type)] ::= constrs::[(String, [Type])] typeName::String
{
  return map(\p::(String, [Type])->(p.fst, constrType(p.snd, typeName)), constrs);
}

@{- Returns the type of a constructor function for a custom type -}
function constrType
Type ::= args::[Type] typeName::String
{
  return constructFuncType(paramsOfTypeArgs(args, 0), customType(typeName));
}

@{- Returns the environment containing the closure values of each constructor for a custom type -}
function constructConstructorEnv
[(String, Value)] ::= constrs::[(String, [Type])] typeName::String env::[(String, Value)]
{
  return map(\p::(String, [Type])->(p.fst, constrValBinding(p, typeName, env)), constrs);
}

@{- Returns the value of a constructor function for a custom type -}
function constrValBinding
Value ::= constr::(String, [Type]) typeName::String env::[(String, Value)]
{
  local name :: String = fst(constr);
  local args :: [Type] = snd(constr);

  local params :: [(String, Type)] = paramsOfTypeArgs(args, 0);

  -- Each parameter should be turned into a var expression
  -- This is because they will all need to be reference as
  -- expressions inside of the base expression at the lowest
  -- level of curried lambdas
  local argExprs :: [Expr] = map(\p::(String, Type) -> var(p.fst), params);
  local baseExpr :: Expr = customTypeExpr(name, argExprs, typeName);
  local lambdaExpr :: Expr = constructLambdas(params, ^baseExpr);
  lambdaExpr.env = env;
  lambdaExpr.typeEnv = [];

  return
    if null(args)
    then customVal(name, [])
    else lambdaExpr.value;
}

@{-
  - Takes a list of types returns a list of named parameters where each type
  - is named arbitrarily. This is done so that the arguments can be passed
  - into the helper functions to construct function values and expressions.
  -}
function paramsOfTypeArgs
[(String, Type)] ::= args::[Type] idx::Integer
{
  return case args of
  | [] -> []
  -- Dollar sign is added to prevent it from colliding with user-defined bindings
  | t::ts -> ("$" ++ toString(idx), t) :: paramsOfTypeArgs(ts, idx + 1)
  end;
}