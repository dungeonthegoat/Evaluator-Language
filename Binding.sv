grammar evaluator;

abstract production var
e::Expr ::= name::String
{
  e.type = lookupType(name, e.typeEnv);
  e.typeErrors := case e.type of
  | errType() -> ["Unbound name '" ++ name ++ "'"]
  | _ -> []
  end;

  e.value = lookup(name, e.env);
}

abstract production letExpr
e::Expr ::= name::String e1::Expr e2::Expr
{
  propagate typeErrors;
  
  e1.env = e.env;
  e2.env = e.env ++ [(name, e1.value)];

  e1.typeEnv = e.typeEnv;
  e2.typeEnv = e.typeEnv ++ [(name, e1.type)];

  e.type = e2.type;
  e.value = e2.value;
}

-- Concrete syntax

concrete production constr_var_c
t::Term_c ::= name::TypeName
{
  t.ast = var(name.lexeme);
}

concrete production var_c
t::Term_c ::= name::Var
{
  t.ast = var(name.lexeme);
}

concrete production let_c
let_e::Expr_c ::= Let var::Var Eq e1::Expr_c In e2::Expr_c
{
  let_e.ast = letExpr(var.lexeme, e1.ast, e2.ast);
}

-- Functions

@{- Looks up a string in an environment and returns its associated value if it exists-}
function lookup
Value ::= s::String e::[(String, Value)]
{
  return case e of
  | (name, val)::rest when name == s -> val
  | _::rest -> lookup(s, rest)
  | _ -> error("Error looking up value of " ++ s)
  end;
}

@{- Looks up a string in a type environment and returns its associated type if it exists-}
function lookupType
Type ::= s::String e::[(String, Type)]
{
  return case e of
  | (name, t)::rest when name == s -> t
  | _::rest -> lookupType(s, rest)
  | _ -> errType()
  end;
}