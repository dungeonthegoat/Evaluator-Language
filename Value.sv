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

-- Each value of a custom type is the name of the type and its arguments to construct it
-- For example, the value of a Shape type could be
-- name = "Rect", args = [width::intVal, height::intVal]
-- For a Tree, it could be
-- name = "Branch", args = [left::customVal, val::intVal, right::customVal]
abstract production customVal
v::Value ::= name::String args::[Value]
{}

-- Functions

@{- Returns the string representation of a value -}
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
  | customVal(n, a) -> case a of
    | [] -> n
    | _ -> n ++ " of " ++ "(" ++ implode(", ", map(toStringValue, a)) ++ ")"
    end
  | _ -> "Value"
  end;
}

@{- Returns the string of cons values separated by commas -}
function toStringList
String ::= h::Value t::Value
{
  return case t of
    | consVal(hd, tl) -> toStringValue(^h) ++ ", " ++ toStringList(^hd, ^tl)
    | emptyListVal() -> toStringValue(^h)
    | _ -> toStringValue(^h) ++ ", " ++ toStringValue(^t)
  end;
}

@{- Returns whether two values are considered equal -}
function equalsValue
Boolean ::= l::Value r::Value
{
  return case l, r of
  | intVal(v1), intVal(v2) -> v1 == v2
  | boolVal(v1), boolVal(v2) -> v1 == v2
  | emptyListVal(), emptyListVal() -> true
  | consVal(h1, t1), consVal(h2, t2) -> equalsValue(^h1, ^h2) && equalsValue(^t1, ^t2)
  | tupleVal(l1, r1), tupleVal(l2, r2) -> equalsValue(^l1, ^l2) && equalsValue(^r1, ^r2)
  | customVal(n1, a1), customVal(n2, a2) -> n1 == n2 && equalsValues(a1, a2)
  | _, _ -> false
  end;
}

@{- Returns whether two lists of values are identical -}
function equalsValues
Boolean ::= l::[Value] r::[Value]
{
  return case l, r of
  | [], [] -> true
  | x::xs, y::ys -> equalsValue(x, y) && equalsValues(xs, ys)
  | _, _ -> false
  end;
}