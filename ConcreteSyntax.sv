grammar evaluator ;

nonterminal Root_c with ast_Root;
synthesized attribute ast_Root :: Root;

concrete production root_c
r::Root_c ::= e::Expr_c
{
  r.ast_Root = root(e.ast); 
}

synthesized attribute ast::Expr;
nonterminal Expr_c with ast;

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
call::Expr_c ::= f::Expr_c '(' arg::Expr_c ')'
{
  call.ast = app(f.ast, arg.ast);
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

concrete production eqOp_c
e::Expr_c ::= e1::Expr_c '==' e2::Expr_c
{
    e.ast = eqOp(e1.ast, e2.ast);
}

concrete production ifThenElse_c
e::Expr_c ::= If b::Expr_c Then v1::Expr_c Else v2::Expr_c
{
    e.ast = ifThenElse(b.ast, v1.ast, v2.ast);
}

concrete production lessThan_c
e::Expr_c ::= l::Expr_c Less r::Expr_c
{
    e.ast = lessThanOp(l.ast, r.ast);
}

concrete production greaterThan_c
e::Expr_c ::= l::Expr_c Greater r::Expr_c
{
    e.ast = greaterThanOp(l.ast, r.ast);
}

concrete production lessThanEq_c
e::Expr_c ::= l::Expr_c LessEq r::Expr_c
{
    e.ast = lessThanEqOp(l.ast, r.ast);
}

concrete production greaterThanEq_c
e::Expr_c ::= l::Expr_c GreaterEq r::Expr_c
{
    e.ast = greaterThanEqOp(l.ast, r.ast);
}

concrete production not_c
e::Expr_c ::= Not b::Expr_c
{
    e.ast = notOp(b.ast);
}

concrete production and_c
e::Expr_c ::= l::Expr_c And r::Expr_c
{
    e.ast = andOp(l.ast, r.ast);
}

concrete production or_c
e::Expr_c ::= l::Expr_c Or r::Expr_c
{
    e.ast = orOp(l.ast, r.ast);
}

concrete production var_c
v_expr::Expr_c ::= name::Var
{
    v_expr.ast = var(name.lexeme);
}

concrete production let_c
let_e::Expr_c ::= 'let' var::Var '=' e1::Expr_c 'in' e2::Expr_c
{
    let_e.ast = letExpr(var.lexeme, e1.ast, e2.ast);
}

concrete production add_c
add_e::Expr_c ::= e1::Expr_c '+' e2::Expr_c
{
    add_e.ast = addOp(e1.ast, e2.ast);
}

concrete production sub_c
sub_e::Expr_c ::= e1::Expr_c '-' e2::Expr_c
{
    sub_e.ast = subOp(e1.ast, e2.ast);
}

concrete production mul_c
mul_e::Expr_c ::= e1::Expr_c '*' e2::Expr_c
{
    mul_e.ast = mulOp(e1.ast, e2.ast);
}

concrete production div_c
div_e::Expr_c ::= e1::Expr_c '/' e2::Expr_c
{
    div_e.ast = divOp(e1.ast, e2.ast);
}

concrete production mod_c
e::Expr_c ::= l::Expr_c Modulo r::Expr_c
{
    e.ast = modOp(l.ast, r.ast);
}