grammar evaluator:infinity;

terminal Inf 'inf' lexer classes {KEYWORDS};
terminal NegInf '-inf' lexer classes {KEYWORDS};

abstract production inf
e::Expr ::= negative::Boolean
{
  e.pp = if negative then "-∞" else "∞";
  e.value = infVal(negative);
}

abstract production infVal
v::Val ::= negative::Boolean
{}

