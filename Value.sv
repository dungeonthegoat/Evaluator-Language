grammar evaluator;

nonterminal Value;

-- Productions

abstract production emptyListVal
v::Value ::=
{}

abstract production consVal
v::Value ::= h::Value t::Value
{}

abstract production boolVal
v::Value ::= b::Boolean
{}

abstract production intVal
v::Value ::= i::Integer
{}

abstract production closureVal
v::Value ::= param::String body::Expr env::[Pair<String Value>]
{}

abstract production recClosureVal
v::Value ::= recName::String param::String body::Expr env::[Pair<String Value>]
{}

abstract production tupleVal
v::Value ::= l::Value r::Value
{}

abstract production customVal
v::Value ::= name::String args::[Value]
{}

-- Functions

@{- Converts a Value to a string -}
function toStringValue
String ::= v::Value
{
  return case v of
  | intVal(i) -> toString(i)
  | closureVal(p, _, _) -> "Closure of " ++ p
  | recClosureVal(f, p, _, _) -> "Recursive closure " ++ f ++ " of " ++ p
  | boolVal(b) -> toString(b)
  | emptyListVal() -> "[]"
  | consVal(h, t) -> "[" ++ toStringList(^h, ^t) ++ "]"
  | tupleVal(l, r) -> "(" ++ toStringValue(^l) ++ ", " ++ toStringValue(^r) ++ ")"
  | _ -> "Value"
  end;
}

function toStringList
String ::= h::Value t::Value
{
  return case t of
    | consVal(hd, tl) -> toStringValue(^h) ++ ", " ++ toStringList(^hd, ^tl)
    | emptyListVal() -> toStringValue(^h)
    | _ -> toStringValue(^h) ++ ", " ++ toStringValue(^t)
  end;
}

function equalsValue
Boolean ::= l::Value r::Value
{
  return case l, r of
  | intVal(v1), intVal(v2) -> v1 == v2
  | boolVal(v1), boolVal(v2) -> v1 == v2
  | emptyListVal(), emptyListVal() -> true
  | consVal(h1, t1), consVal(h2, t2) -> equalsValue(^h1, ^h2) && equalsValue(^t1, ^t2)
  | tupleVal(l1, r1), tupleVal(l2, r2) -> equalsValue(^l1, ^l2) && equalsValue(^r1, ^r2)
  | _, _ -> false
  end;
}