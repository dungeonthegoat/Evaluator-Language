grammar evaluator;

nonterminal TypedVar_c with name, type_ast;
synthesized attribute name :: String;

concrete production typedVar_c
tv::TypedVar_c ::= t::Type_c v::Var
{
  tv.name = v.lexeme;
  tv.type_ast = t.type_ast;
}

nonterminal TypedVarList_c with vars;
synthesized attribute vars :: [(String, Type)];

concrete production singleTypedVar_c
tvl::TypedVarList_c ::= tv::TypedVar_c
{
  tvl.vars = [(tv.name, tv.type_ast)];
}

concrete production typedVarList_c
tvl::TypedVarList_c ::= tv::TypedVar_c rest::TypedVarList_c
{
  tvl.vars = (tv.name, tv.type_ast) :: rest.vars;
}