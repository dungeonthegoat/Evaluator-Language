grammar evaluator;

abstract production emptyExpr
e::Expr ::=
{
  e.value = emptyListVal();
}

abstract production consExpr
e::Expr ::= h::Expr t::Expr
{
  h.env = e.env;
  t.env = e.env;
  e.value = consVal(h.value, t.value);
}

abstract production appendExpr
e::Expr ::= l::Expr r::Expr
{
  l.env = e.env;
  r.env = e.env;

  e.value = appendLists(l.value, r.value);
}

-- Concrete syntax

concrete production emptyList_c
e::Expr_c ::= '[' ']'
{
    e.ast = emptyExpr();
}

concrete production cons_c
e::Expr_c ::= h::Expr_c ConsOp t::Expr_c
{
    e.ast = consExpr(h.ast, t.ast);
}

concrete production append_c
e::Expr_c ::= l::Expr_c Append r::Expr_c
{
  e.ast = appendExpr(l.ast, r.ast);
}

nonterminal ExprList_c with ast_ExprList;
synthesized attribute ast_ExprList :: Expr;

concrete production singleExprList_c
es::ExprList_c ::= e::Expr_c
{
  es.ast_ExprList = consExpr(e.ast, emptyExpr());
}

concrete production multiExprList_c
es::ExprList_c ::= e::Expr_c Sep rest::ExprList_c
{
  es.ast_ExprList = consExpr(e.ast, rest.ast_ExprList);
}

concrete production list_c
e::Expr_c ::= '[' list::ExprList_c ']'
{
  e.ast = list.ast_ExprList;
}

-- Functions

function appendLists
Value ::= l1::Value l2::Value
{
  return case l1 of
    | emptyListVal() -> new(l2)
    | consVal(h, t) -> consVal(new(h), appendLists(new(t), new(l2)))
    | _ -> error("Type error when appending lists")
  end;
}