grammar evaluator;

nonterminal Type;

abstract production intType
t::Type ::=
{}

abstract production boolType
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

-- Concrete syntax

nonterminal Type_c with type_ast;
synthesized attribute type_ast :: Type;

concrete productions t::Type_c
| IntT { t.type_ast = intType(); }
| BoolT { t.type_ast = boolType(); }
| AnyT { t.type_ast = anyType(); }
| '[' t1::Type_c ']' { t.type_ast = listType(t1.type_ast); }
| p::Type_c Arrow r::Type_c { t.type_ast = funcType(p.type_ast, r.type_ast); }
| '(' t1::Type_c ')' { t.type_ast = t1.type_ast; }
| '(' l::Type_c ',' r::Type_c ')' { t.type_ast = tupleType(l.type_ast, r.type_ast); }

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
  | boolType() -> case t2 of
    | anyType() -> true
    | boolType() -> true
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
  | boolType() -> "bool"
  | errType() -> "error"
  | listType(innerT) -> "[" ++ toStringType(^innerT) ++ "]"
  | funcType(paramT, returnT) -> "(" ++ toStringType(^paramT) ++ " -> " ++ toStringType(^returnT) ++ ")"
  | anyType() -> "any"
  | tupleType(l, r) -> "(" ++ toStringType(^l) ++ ", " ++ toStringType(^r) ++ ")"
  | customType(n) -> n
  | _ -> "?"
  end;
}