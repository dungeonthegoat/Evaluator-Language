grammar evaluator;

abstract production untypedFunction
e::Expr ::= name::String params::[String] body::Expr rest::Expr
{
    local paramTypes :: [(String, Type)] = constructParameterTypeIDs(params, e.varID);
    local retType :: Type = inferredType(e.varID + length(params));

    forwards to letRecExpr(
        name,
        head(paramTypes).fst,
        head(paramTypes).snd,
        constructFuncType(tail(paramTypes), ^retType),
        constructLambdas(tail(paramTypes), ^body),
        ^rest
    );
}

abstract production letRecExpr
e::Expr ::= name::String param::String paramType::Type returnType::Type body::Expr e2::Expr
{
    propagate typeErrors;

    local selfType :: Type = funcType(^paramType, ^returnType);

    body.typeEnv = (name, ^selfType) :: (param, ^paramType) :: e.typeEnv;
    body.env = e.env;
    body.varID = case returnType of
    | inferredType(id) -> id + 1
    | _ -> e.varID
    end;
    body.substMap = e.substMap;

    local retSubst :: [(Integer, Type)] = unify(body.type, ^returnType, body.newSubstMap);
    local newBodyType :: Type = applySubstitution(body.type, retSubst);
    local newReturnType :: Type = applySubstitution(^returnType, retSubst);
    local newSelfType :: Type = applySubstitution(^selfType, retSubst);

    e2.typeEnv = (name, ^newSelfType) :: e.typeEnv;
    e2.env = (name, recClosureVal(name, param, ^body, e.env)) :: e.env;
    e2.varID = body.newVarID;
    e2.substMap = retSubst;

    e.type = e2.type;
    e.value = e2.value;
    e.newVarID = e2.newVarID;
    e.newSubstMap = e2.newSubstMap;

    local retTypeError :: [String] = case newBodyType, newReturnType of
    | t1, t2 when equalsType(^t1, ^t2) -> []
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
  
  b.typeEnv = (param, ^paramType) :: e.typeEnv;
  b.varID = e.varID;
  b.substMap = e.substMap;

  e.type = funcType(^paramType, b.type);
  e.value = closureVal(param, ^b, e.env);
  e.newVarID = b.newVarID;
  e.newSubstMap = b.newSubstMap;
}

abstract production app
e::Expr ::= func::Expr arg::Expr
{
    propagate env, typeEnv, typeErrors;

    func.varID = e.varID;
    arg.varID = func.newVarID;
    local retVar :: Type = inferredType(arg.newVarID);
    e.newVarID = arg.newVarID + 1;

    func.substMap = e.substMap;
    arg.substMap = func.newSubstMap;
    e.newSubstMap = unify(func.type, funcType(arg.type, ^retVar), arg.newSubstMap);

    local resolvedFuncType :: Type = applySubstitution(func.type, e.newSubstMap);
    local resolvedArgType :: Type = applySubstitution(arg.type, e.newSubstMap);

    local paramType :: Type = case resolvedFuncType of
    | funcType(p, _) -> ^p
    | _ -> errType()
    end;

    local funcTypeError :: [String] = case resolvedFuncType of
    | funcType(_, _) -> []
    | errType() -> []
    | _ -> ["Attempted application of a non-function"]
    end;

    local argMatchParamError :: [String] =
    if equalsType(^resolvedArgType, ^paramType) || equalsType(^resolvedArgType, errType())
    then []
    else [
            "Argument type " ++
            toStringType(^resolvedArgType) ++
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

    e.type = applySubstitution(^retVar, e.newSubstMap);
    e.value = body.value;

    e.typeErrors <- funcTypeError ++ argMatchParamError;
}

-- Concrete syntax

concrete production lambda_c
e::Expr_c ::= Lambda param::Var Colon t::Type_c Arrow body::Expr_c
{
  e.ast = lambda(param.lexeme, t.type_ast, body.ast);
}

-- Untyped function, names.head represents the function name,
-- the rest are parameter names
concrete production untyped_func
e::Expr_c ::= Let Inf names::VarList_c Eq body::Expr_c In rest::Expr_c
{
    e.ast = untypedFunction(
        head(names.untyped_vars),
        tail(names.untyped_vars),
        body.ast,
        rest.ast
    );
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

nonterminal VarList_c with untyped_vars;
synthesized attribute untyped_vars :: [String];

concrete production singleVar_c
vl::VarList_c ::= v::Var
{
    vl.untyped_vars = [v.lexeme];
}

concrete production varList_c
vl::VarList_c ::= v::Var rest::VarList_c
{
    vl.untyped_vars = v.lexeme :: rest.untyped_vars;
}

-- Functions

@{- Constructs the inferred type IDs for a list of untyped parameters -}
function constructParameterTypeIDs
[(String, Type)] ::= params::[String] nextId::Integer
{
    return case params of
    | [] -> []
    | param::rest ->
        (param, inferredType(nextId))::constructParameterTypeIDs(rest, nextId + 1)
    end;
}

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