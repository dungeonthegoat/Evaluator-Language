grammar evaluator ;

nonterminal Root_c with ast_Root;
synthesized attribute ast_Root :: Root;

concrete production root_c
r::Root_c ::= e::Expr_c
{
  r.ast_Root = root(e.ast); 
}

nonterminal Expr_c with ast;
synthesized attribute ast::Expr;

nonterminal Pat_c with ast_Pat;
synthesized attribute ast_Pat :: Pattern;

-- Productions

concrete production let_rec_c
e::Expr_c ::= Let Rec recName::Var param::Var '=' body::Expr_c 'in' e2::Expr_c
{
    e.ast = letRecExpr(recName.lexeme, param.lexeme, body.ast, e2.ast);
}

concrete production lambda_c
lam::Expr_c ::= LambdaParam param::Var Arrow body::Expr_c
{
    lam.ast = lambda(param.lexeme, body.ast);
}

concrete production app_c
e::Expr_c ::= f::Expr_c '(' arg::Expr_c ')'
{
    e.ast = app(f.ast, arg.ast);
}

concrete production paren_c
e::Expr_c ::= '(' e1::Expr_c ')'
{
    e.ast = e1.ast;
}

concrete production numLit_c
num_e::Expr_c ::= i::IntLit
{
    num_e.ast = numLit(toInteger(i.lexeme));
}

concrete production true_c
e::Expr_c ::= True
{
    e.ast = boolLit(true);
}

concrete production false_c
e::Expr_c ::= False
{
    e.ast = boolLit(false);
}

concrete production var_c
v_expr::Expr_c ::= name::Var
{
    v_expr.ast = var(name.lexeme);
}

concrete production let_c
let_e::Expr_c ::= Let var::Var '=' e1::Expr_c 'in' e2::Expr_c
{
    let_e.ast = letExpr(var.lexeme, e1.ast, e2.ast);
}