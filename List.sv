grammar evaluator;

abstract production emptyExpr
e::Expr ::=
{
  e.type = listType(anyType());
  e.value = emptyListVal();
}

abstract production consExpr
e::Expr ::= h::Expr t::Expr
{
  propagate env, typeEnv, typeErrors;

  e.type = listType(h.type);
  e.value = consVal(h.value, t.value);

  e.typeErrors <- case t.type of
  | listType(innerT) ->
    if equalsType(h.type, ^innerT)
    then []
    else ["All elements of a list must match type"]
  | _ -> ["Cannot cons with a non-list"]
  end;
}

abstract production appendExpr
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;

  e.type = l.type;
  e.value = appendLists(l.value, r.value);

  e.typeErrors <- case l.type, r.type of
  | listType(t1), listType(t2) -> []
  | errType(), _ -> []
  | _, errType() -> []
  | _, _ -> ["Invalid operand types when appending lists"]
  end;
}

abstract production listCompExpr
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;

  e.type = listType(intType());
  e.value = constructIntList(constructCompList(getInt(l.value), getInt(r.value)));

  e.typeErrors <- case l.type, r.type of
  | intType(), intType() -> []
  | errType(), _ -> []
  | _, errType() -> []
  | _, _ -> ["Invalid types in list comprehension"]
  end;
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

concrete production listComp_c
e::Expr_c ::= '[' l::Expr_c Dots r::Expr_c ']'
{
  e.ast = listCompExpr(l.ast, r.ast);
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
  | emptyListVal() -> ^l2
  | consVal(h, t) -> consVal(^h, appendLists(^t, ^l2))
  | _ -> error("Type error when appending lists")
  end;
}

function constructIntList
Value ::= l::[Integer]
{
  return case l of
  | [] -> emptyListVal()
  | h::t -> consVal(intVal(h), constructIntList(t))
  end;
}

function constructCompList
[Integer] ::= start::Integer finish::Integer
{
  return 
    if start == finish
    then [finish]
    else start :: constructCompList(start + if finish > start then 1 else -1, finish);
}