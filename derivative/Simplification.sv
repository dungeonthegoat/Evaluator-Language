grammar evaluator:derivative;

import evaluator;

synthesized attribute simple :: Expr occurs on Expr;

aspect production numLit
e::Expr ::= i::Integer
{
  e.simple = numLit(i);
}

aspect production var
e::Expr ::= name::String
{
  e.simple = var(name);
}

aspect production addOp
e::Expr ::= l::Expr r::Expr
{
  e.simple = case l.simple, r.simple of
  | numLit(0), x -> x
  | x, numLit(0) -> x
  | numLit(x), numLit(y) -> numLit(x + y)
  | var(x), var(y) when x == y -> mulOp(numLit(2), var(x)) -- x + x = 2x
  | _, _ -> addOp(l.simple, r.simple)
  end;
}

aspect production subOp
e::Expr ::= l::Expr r::Expr
{
  e.simple = case l.simple, r.simple of
  | numLit(0), x -> x
  | x, numLit(0) -> x
  | numLit(x), numLit(y) -> numLit(x - y)
  | var(x), var(y) when x == y -> numLit(0) -- x - x = 0
  | _, _ -> subOp(l.simple, r.simple)
  end;
}

aspect production mulOp
e::Expr ::= l::Expr r::Expr
{
  e.simple = case l.simple, r.simple of
  | numLit(0), _ -> numLit(0)
  | _, numLit(0) -> numLit(0)
  | numLit(1), x -> x
  | x, numLit(1) -> x
  | numLit(x), numLit(y) -> numLit(x * y)
  | var(x), var(y) when x == y -> powOp(l.simple, numLit(2)) -- x * x = x^2
  | addOp(x, y), z -> addOp(mulOp(^x, z), mulOp(^y, z)) -- (x + y) * z = xz + yz
  | z, addOp(x, y) -> addOp(mulOp(^x, z), mulOp(^y, z)) -- z * (x + y) = xz + yz
  | addOp(a, b), addOp(c, d) -> -- (a + b) * (c + d) = ac + ad + bc + bd
    addOp(addOp(mulOp(^a, ^c), mulOp(^a, ^d)), addOp(mulOp(^b, ^c), mulOp(^b, ^d)))
  | _, _ -> mulOp(l.simple, r.simple)
  end;
}

aspect production powOp
e::Expr ::= l::Expr r::Expr
{
  e.simple = case l.simple, r.simple of
  | numLit(0), x -> numLit(0)
  | x, numLit(0) -> numLit(1)
  | numLit(1), x -> numLit(1)
  | x, numLit(1) -> x
  | numLit(x), numLit(y) -> numLit(pow(x, y))
  | mulOp(x, y), z -> -- (xy)^z = x^z * y^z
    mulOp(powOp(^x, z), powOp(^y, z))
  | _, _ -> powOp(l.simple, r.simple)
  end;
}