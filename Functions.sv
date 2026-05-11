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

concrete production pipe
e::Expr_c ::= arg::Expr_c Pipe func::Expr_c
{
  e.ast = app(func.ast, arg.ast);
}


nonterminal TypedVar_c with name, type_ast;
synthesized attribute name :: String;

concrete production typedVar_c
tv::TypedVar_c ::= t::Type_c v::Var
{
  tv.name = v.lexeme;
  tv.type_ast = t.type_ast;
}

concrete production typedVarParen_c
tv::TypedVar_c ::= LeftParen t::TypedVar_c RightParen
{
  tv.name = t.name;
  tv.type_ast = t.type_ast;
}

nonterminal TypedVarList_c with vars;
synthesized attribute vars :: [(String, Type)];

concrete production singleTypedVar_c
tvl::TypedVarList_c ::= tv::TypedVar_c
{
  tvl.vars = [(tv.name, tv.type_ast)];
}

concrete production typedVarList_c
tvl::TypedVarList_c ::= tv::TypedVar_c rest::TypedVarList_c
{
  tvl.vars = (tv.name, tv.type_ast) :: rest.vars;
}

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