grammar evaluator;

nonterminal Type;

abstract production intType
t::Type ::=
{}

abstract production floatType
t::Type ::=
{}

abstract production boolType
t::Type ::=
{}

abstract production stringType
t::Type ::=
{}

abstract production listType
t::Type ::= varType::Type
{}

abstract production anyType
t::Type ::=
{}

abstract production funcType
t::Type ::= paramType::Type returnType::Type
{}

abstract production errType
t::Type ::=
{}

abstract production tupleType
t::Type ::= l::Type r::Type
{}

abstract production customType
t::Type ::= name::String
{}

abstract production typeDeclaration
e::Expr ::= name::String constructors::[(String, [Type])] rest::Expr
{
  propagate typeErrors;

  -- Each type gets its own constructor function which is added into the env
  rest.env = constructConstructorEnv(constructors, name, e.env) ++ e.env;
  rest.typeEnv = constructConstructorTypeEnv(constructors, name) ++ e.typeEnv;

  e.type = rest.type;
  e.value = rest.value;

  e.pp = "type " ++ name ++ " = " ++ 
    implode(" | ",
      map(\constr::(String, [Type]) ->
        fst(constr) ++ " of " ++ "(" ++ implode(", ", map(toStringType, snd(constr))) ++ ")"
      , constructors)
    ) ++ " in\n" ++ rest.pp;
}

abstract production typeCast
e::Expr ::= t::Type e1::Expr
{
  propagate env, typeEnv, typeErrors;

  local typeCastError :: [String] =
    case t, e1.type of
    | t1, t2 when equalsType(^t1, t2) -> []
    | intType(), floatType() -> []
    | floatType(), intType() -> []
    | t1, t2 -> ["Cannot type cast " ++ toStringType(^t1) ++ " with " ++ toStringType(t2)]
    end;

  e.type =
    case t, e1.type of
    | t1, t2 when equalsType(^t1, t2) -> ^t1
    | intType(), floatType() -> intType()
    | floatType(), intType() -> floatType()
    | _, _ -> errType()
    end;
  
  e.value = 
    case t, e1.value of
    | intType(), floatVal(f) -> intVal(toInteger(f))
    | floatType(), intVal(i) -> floatVal(toFloat(i))
    | _, _ -> e1.value
    end;
  
  e.typeErrors <- typeCastError;
}

-- Concrete syntax

nonterminal Type_c with type_ast;
synthesized attribute type_ast :: Type;

concrete productions t::Type_c
| IntT { t.type_ast = intType(); }
| FloatT { t.type_ast = floatType(); }
| BoolT { t.type_ast = boolType(); }
| StringT { t.type_ast = stringType(); }
| AnyT { t.type_ast = anyType(); }
| LeftBracket t1::Type_c RightBracket { t.type_ast = listType(t1.type_ast); }
| LeftParen p::Type_c Arrow r::Type_c RightParen { t.type_ast = funcType(p.type_ast, r.type_ast); }
| LeftParen t1::Type_c RightParen { t.type_ast = t1.type_ast; }
| LeftParen l::Type_c Sep r::Type_c RightParen { t.type_ast = tupleType(l.type_ast, r.type_ast); }
| v::TypeName { t.type_ast = customType(v.lexeme); }

concrete production typeCastExpr
e::Expr_c ::= Less t::Type_c Greater e1::Expr_c
{
  e.ast = typeCast(t.type_ast, e1.ast);
}

-- Functions

@{-Compares two types to see if they are equivalent-}
function equalsType
Boolean ::= t1::Type t2::Type
{
  return case t1 of
  | intType() -> case t2 of
    | anyType() -> true
    | intType() -> true
    | _ -> false
    end
  | floatType() -> case t2 of
    | anyType() -> true
    | floatType() -> true
    | _ -> false
    end
  | boolType() -> case t2 of
    | anyType() -> true
    | boolType() -> true
    | _ -> false
    end
  | stringType() -> case t2 of
    | anyType() -> true
    | stringType() -> true
    | _ -> false
    end
  | errType() -> case t2 of
    | anyType() -> true
    | errType() -> true
    | _ -> false
    end
  | listType(innerT1) -> case t2 of
    | anyType() -> true
    | listType(innerT2) -> equalsType(^innerT1, ^innerT2)
    | _ -> false
    end
  | funcType(paramT1, returnT1) -> case t2 of
    | anyType() -> true
    | funcType(paramT2, returnT2) -> equalsType(^paramT1, ^paramT2) && equalsType(^returnT1, ^returnT2)
    | _ -> false
    end
  | tupleType(l1, r1) -> case t2 of
    | anyType() -> true
    | tupleType(l2, r2) -> equalsType(^l1, ^l2) && equalsType(^r1, ^r2)
    | _ -> false
    end
  | customType(n1) -> case t2 of
    | anyType() -> true
    | customType(n2) -> n1 == n2
    | _ -> false
    end
  | anyType() -> true
  | _ -> false
  end;
}

function toStringType
String ::= t::Type
{
  return case t of
  | intType() -> "int"
  | floatType() -> "float"
  | boolType() -> "bool"
  | stringType() -> "string"
  | errType() -> "error"
  | listType(innerT) -> "[" ++ toStringType(^innerT) ++ "]"
  | funcType(paramT, returnT) -> "(" ++ toStringType(^paramT) ++ " -> " ++ toStringType(^returnT) ++ ")"
  | anyType() -> "any"
  | tupleType(l, r) -> "(" ++ toStringType(^l) ++ ", " ++ toStringType(^r) ++ ")"
  | customType(n) -> n
  | _ -> "?"
  end;
}

@{- Turns each constructor into a funcType representing its constructor function's type, and returns the list of such constructor types -}
function constructConstructorTypeEnv
[(String, Type)] ::= constrs::[(String, [Type])] typeName::String
{
  return map(\p::(String, [Type])->(p.fst, getConstructorType(p.snd, typeName)), constrs);
}

@{- Returns the type of a constructor function for a custom type -}
function getConstructorType
Type ::= args::[Type] typeName::String
{
  return constructFuncType(paramsOfTypeArgs(args, 0), customType(typeName));
}

@{- Returns the environment containing the closure values of each constructor for a custom type -}
function constructConstructorEnv
[(String, Value)] ::= constrs::[(String, [Type])] typeName::String env::[(String, Value)]
{
  return map(\p::(String, [Type])->(p.fst, getConstructorValue(p, typeName, env)), constrs);
}

@{- Returns the value of a constructor function for a custom type -}
function getConstructorValue
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