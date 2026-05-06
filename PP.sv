synthesized attribute pp :: String;

aspect pp on Expr of
-- Binding
| var(name) -> name
| letExpr(name, e1, e2) -> "let " ++ name ++ " = " ++ e1.pp ++ " in\n" ++ e2.pp
-- Boolean
| boolLit(b) -> if b then "true" else "false"
| eqOp(l, r) -> "(" ++ l.pp ++ " == " ++ r.pp ++ ")"
| ifThenElse(b, v1, v2) -> "if " ++ b.pp ++ "\n\tthen " ++ v1.pp ++ "\n\telse " ++ v2.pp
| andOp(l, r) -> "(" ++ l.pp ++ " && " ++ r.pp ++ ")"
| orOp(l, r) -> "(" ++ l.pp ++ " || " ++ r.pp ++ ")"
| notOp(b) -> "(!" ++ b.pp ++ ")"
-- Functions
| letRecExpr(name, p, pt, rt, b, e2) -> 
  name ++ "(" ++ p ++ " : " ++ toStringType(^pt) ++ ")" ++ " : " ++ toStringType(^rt) ++ " -> " ++ b.pp ++ " in\n" ++ e2.pp
| lambda(p, pt, b) -> "(λ" ++ p ++ ":" ++ toStringType(^pt) ++ " → " ++ b.pp ++ ")"
| app(f, a) -> f.pp ++ "(" ++ a.pp ++ ")"
-- Integer
| numLit(i) -> toString(i)
| lessThanOp(l, r) -> "(" ++ l.pp ++ " < " ++ r.pp ++ ")"
| greaterThanOp(l, r) -> "(" ++ l.pp ++ " > " ++ r.pp ++ ")"
| lessThanEqOp(l, r) -> "(" ++ l.pp ++ " <= " ++ r.pp ++ ")"
| greaterThanEqOp(l, r) -> "(" ++ l.pp ++ " >= " ++ r.pp ++ ")"
| addOp(l, r) -> "(" ++ l.pp ++ " + " ++ r.pp ++ ")"
| subOp(l, r) -> "(" ++ l.pp ++ " - " ++ r.pp ++ ")"
| mulOp(l, r) -> "(" ++ l.pp ++ " * " ++ r.pp ++ ")"
| divOp(l, r) -> "(" ++ l.pp ++ " / " ++ r.pp ++ ")"
| powOp(l, r) -> l.pp ++ "^" ++ r.pp
| modOp(l, r) -> "(" ++ l.pp ++ " % " ++ r.pp ++ ")"
-- List
| emptyExpr() -> "[]"
| consExpr(h, t) -> h.pp ++ "::" ++ t.pp
| appendExpr(l, r) -> "(" ++ l.pp ++ " @ " ++ r.pp ++ ")"
| listCompExpr(l, r) -> "[" ++ l.pp ++ "..." ++ r.pp ++ "]"
-- Match
| matchExpr(t, c) -> "case " ++ t.pp ++ " of\n" ++ c.pp
-- Tuple
| tupleExpr(l, r) -> "(" ++ l.pp ++ ", " ++ r.pp ++ ")"
end;

aspect pp on MatchCase of
| matchCase(p, e) -> "| " ++ p.pp ++ " -> " ++ e.pp
end;

aspect pp on MatchCases of
| consCases(h, t) -> "\t" ++ h.pp ++ "\n" ++ t.pp
end;

aspect pp on Pattern of
| emptyListPat() -> "[]"
| consPat(p1, p2) -> p1.pp ++ "::" ++ p2.pp
| tuplePat(l, r) -> "(" ++ l.pp ++ ", " ++ r.pp ++ ")"
| varPat(name) -> name
| customPat(name, ps) -> name ++ "(" ++ implode(", ", 
  map(\pat::Pattern -> pat.pp, ps)
  ) ++ ")"
end;