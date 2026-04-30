abstract production tupleExpr
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;

  e.type = tupleType(l.type, r.type);
  e.value = tupleVal(l.value, r.value);
}

concrete production tuple_c
e::Expr_c ::= '(' l::Expr_c ',' r::Expr_c ')'
{
  e.ast = tupleExpr(l.ast, r.ast);
}