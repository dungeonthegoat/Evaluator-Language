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

abstract production customTypeExpr
e::Expr ::= name::String args::[Expr] typeName::String
{
  -- Since Silver doesn't support propagating over lists of expressions (possible extension?),
  -- we have to do it manually
  local decoratedArgs :: [Decorated Expr] = 
    map(\ex::Expr -> decorate ex with {
        env = e.env;
        typeEnv = e.typeEnv;
      }, args
    );

  e.type = customType(typeName);
  e.value = customVal(name, map(\a::Decorated Expr -> a.value, decoratedArgs));
  e.typeErrors <- foldr(append, [], map(\a::Decorated Expr -> a.typeErrors, decoratedArgs));
  e.pp = name ++ "(" ++ implode(", ", map(\a::Decorated Expr -> a.pp, decoratedArgs)) ++ ")";
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

concrete production type_declaration_c
e::Expr_c ::= ADT name::TypeName Eq cs::ConstrList_c In rest::Expr_c
{
  e.ast = typeDeclaration(name.lexeme, cs.constrs, rest.ast);
}

nonterminal TypeList_c with types;
synthesized attribute types :: [Type];

concrete productions t::TypeList_c
| t1::Type_c { t.types = [t1.type_ast]; }
| t1::Type_c ',' ts::TypeList_c { t.types = t1.type_ast :: ts.types; }

nonterminal Constr_c with type_name, types;
synthesized attribute type_name :: String;

concrete production constr_c
c::Constr_c ::= name::TypeName '(' ts::TypeList_c ')'
{
  c.type_name = name.lexeme;
  c.types = ts.types;
}

nonterminal ConstrList_c with constrs;
synthesized attribute constrs :: [(String, [Type])];

concrete productions cs::ConstrList_c
| c::Constr_c { cs.constrs = [(c.type_name, c.types)]; }
| c::Constr_c Bar rest::ConstrList_c { cs.constrs = (c.type_name, c.types) :: rest.constrs; }