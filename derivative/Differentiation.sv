grammar evaluator:derivative;

import evaluator;

terminal Prime  '`'  precedence = 15, association = left;

inherited attribute differential :: String occurs on Expr;
synthesized attribute derivative :: Expr occurs on Expr;

aspect production addOp e::Expr ::= l::Expr r::Expr { propagate differential; }
aspect production subOp e::Expr ::= l::Expr r::Expr { propagate differential; }
aspect production mulOp e::Expr ::= l::Expr r::Expr { propagate differential; }
aspect production divOp e::Expr ::= l::Expr r::Expr { propagate differential; }
aspect production powOp e::Expr ::= l::Expr r::Expr { propagate differential; }

aspect derivative on e::Expr of
| intLit(_) -> intLit(0)
| addOp(l, r) -> addOp(l.derivative, r.derivative)
| subOp(l, r) -> subOp(l.derivative, r.derivative)
| mulOp(l, r) -> addOp(mulOp(^l, r.derivative), mulOp(^r, l.derivative))
| divOp(l, r) -> divOp(subOp(mulOp(l.derivative, ^r), mulOp(^l, r.derivative)), powOp(^r, intLit(2)))
| powOp(l, r) -> mulOp(mulOp(^r, powOp(^l, intLit(getInt(r.value) - 1))), l.derivative)
| var(name) -> 
  if name == e.differential
  then intLit(1)
  else intLit(0)
| _ -> intLit(0)
end;

abstract production primeOp
e::Expr ::= f::Expr
{
  propagate env, typeEnv, typeErrors;

  local funcTypeError :: [String] = case f.type of
  | funcType(_, _) -> []
  | _ -> ["Invalid or erroneous operand for differentiating"]
  end;

  local param :: String = case f.value of
  | closureVal(p, _, _) -> p
  | recClosureVal(_, p, _, _) -> p
  | _ -> error("Differentiation failed; function does not match closure type")
  end;

  local body :: Expr = case f.value of
  | closureVal(_, b, _) -> ^b
  | recClosureVal(_, _, b, _) -> ^b
  | _ -> error("Differentiation failed; function does not match closure type")
  end;

  body.differential = param;
  body.env = f.env;
  body.typeEnv = f.typeEnv;

  local attribute diffLambda :: Expr;
  diffLambda = lambda(param, intType(), body.derivative.simple);
  diffLambda.env = e.env;
  diffLambda.typeEnv = e.typeEnv;

  forwards to ^diffLambda;
  e.typeErrors <- funcTypeError ++ diffLambda.typeErrors;
}

concrete production prime_c
e::Expr_c ::= f::Expr_c Prime
{
  e.ast = primeOp(f.ast);
}