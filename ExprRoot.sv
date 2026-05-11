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

  e.env = [];
  e.typeEnv = [];

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

abstract production stringLit
e::Expr ::= str::String
{
  e.type = stringType();
  e.value = stringVal(str);
}

abstract production concatenate
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;

  local typeMatchError :: [String] =
    case l.type, r.type of
    | stringType(), stringType() -> []
    | errType(), _ -> []
    | _, errType() -> []
    | _, _ -> ["Cannot concatenate non-strings"]
    end;

  e.type =
    case l.type, r.type of
    | stringType(), stringType() -> stringType()
    | _, _ -> errType()
    end;
  
  e.value = stringVal(getStringVal(l.value) ++ getStringVal(r.value));
  e.typeErrors <- typeMatchError;
}

-- Concrete syntax

@@{-
  - Concrete syntazx evaluation is split up into three main precedence levels:
  - 1. Expr_c (lowest precedence) (everything else)
  - 2. App_c  (middle precedence) (function application)
  - 3. Term_c (highest precedence) (irreducible values like literals)

  - An Expr_c or App_c can also be a Term_c, but not the other way around
  - A function can only apply from an App_c onto a Term_c (this results in a new App_c)
    - Example: add 1 2
    You can attempt to evaluate it as add (1 2), where 1 gets lifted from a
    Term_c to an App_c. However, by doing so, you would be left with

                          add::App_c (1 2)::App_c
    
    which is not a concrete production. Therefore, it must be evaluated in
    a left-associative way.
-}

nonterminal Root_c with astRoot;
nonterminal Expr_c with ast;
nonterminal Term_c with ast;

synthesized attribute astRoot::Root;
synthesized attribute ast::Expr;


concrete production root_c
r::Root_c ::= e::Expr_c
{
  r.astRoot = root(e.ast); 
}

concrete productions e::Expr_c
| a::App_c { e.ast = a.ast; }
| If c::Expr_c Then e1::Expr_c Else e2::Expr_c { e.ast = ifThenElse(c.ast, e1.ast, e2.ast); }
| l::Expr_c Concat r::Expr_c { e.ast = concatenate(l.ast, r.ast); }

concrete production paren_c
t::Term_c ::= LeftParen e::Expr_c RightParen
{
  t.ast = e.ast;
}

concrete production type_declaration_c
e::Expr_c ::= ADT name::TypeName Eq cs::ConstrList_c In rest::Expr_c
{
  e.ast = typeDeclaration(name.lexeme, cs.constrs, rest.ast);
}

concrete productions t::Term_c
| s::StringLitT { t.ast = stringLit(implode("", getInnerString(explode("", substring(1, length(s.lexeme) - 1, s.lexeme))))); }


nonterminal TypeList_c with types;
synthesized attribute types :: [Type];

concrete productions t::TypeList_c
| t1::Type_c { t.types = [t1.type_ast]; }
| t1::Type_c Sep ts::TypeList_c { t.types = t1.type_ast :: ts.types; }

nonterminal Constr_c with type_name, types;
synthesized attribute type_name :: String;

concrete production constr_c
c::Constr_c ::= name::TypeName LeftParen ts::TypeList_c RightParen
{
  c.type_name = name.lexeme;
  c.types = ts.types;
}

nonterminal ConstrList_c with constrs;
synthesized attribute constrs :: [(String, [Type])];

concrete productions cs::ConstrList_c
| c::Constr_c { cs.constrs = [(c.type_name, c.types)]; }
| c::Constr_c Bar rest::ConstrList_c { cs.constrs = (c.type_name, c.types) :: rest.constrs; }


function getInnerString
[String] ::= s::[String]
{
  return case s of
  | [] -> []
  | "\\"::rest -> getInnerString(rest)
  | c::rest -> c::getInnerString(rest)
  end;
}


function getStringVal
String ::= v::Value
{
  return case v of
  | stringVal(s) -> s
  | _ -> ""
  end;
}