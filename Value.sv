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
  | consVal(h, t) -> "[" ++ toStringList(new(h), new(t)) ++ "]"
  | _ -> "Value"
  end;
}

function toStringList
String ::= h::Value t::Value
{
  return case t of
    | consVal(hd, tl) -> toStringValue(new(h)) ++ ", " ++ toStringList(new(hd), new(tl))
    | emptyListVal() -> toStringValue(new(h))
    | _ -> toStringValue(new(h)) ++ ", " ++ toStringValue(new(t))
  end;
}