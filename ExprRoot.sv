grammar evaluator;

nonterminal Root with value, pp, type, typeErrors;
nonterminal Expr with value, pp, env, type, typeErrors, typeEnv;

inherited attribute env :: [(String, Value)];
inherited attribute typeEnv :: [(String, Type)];
monoid attribute typeErrors :: [String] with [], ++;
synthesized attribute value :: Value;
synthesized attribute type :: Type;

-- Default

aspect default production 
e::Expr ::=
{
  e.typeErrors := [];
}

-- Abstract syntax

abstract production root
r::Root ::= e::Expr
{
  propagate typeErrors;

  e.typeEnv = [];
  e.env = [];

  r.pp = e.pp;
  r.value = e.value;
  r.type = e.type;
}

-- Concrete syntax

nonterminal Root_c with astRoot;
nonterminal Expr_c with ast;

synthesized attribute astRoot::Root;
synthesized attribute ast::Expr;

concrete production root_c
r::Root_c ::= e::Expr_c
{
  r.astRoot = root(e.ast); 
}

concrete production paren_c
e::Expr_c ::= '(' e1::Expr_c ')'
{
  e.ast = e1.ast;
}