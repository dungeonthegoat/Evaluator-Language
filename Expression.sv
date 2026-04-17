grammar evaluator;

-- Nonterminals

nonterminal Root with value;
nonterminal Expr with value, env;

-- Attributes

inherited attribute env :: [Pair<String Value>];
synthesized attribute value :: Value;

-- Root Production

abstract production root
r::Root ::= e::Expr
{
  e.env = [];
  r.value = e.value;
}

-- Operations

abstract production matchExpr
e::Expr ::= target::Expr cases::MatchCases
{
  target.env = e.env;
  cases.env = e.env;
  cases.matchingVal = target.value;
  
  e.value = cases.value;
}

abstract production var
e::Expr ::= name::String
{
  e.value = lookup(name, e.env);
}

abstract production letExpr
e::Expr ::= name::String e1::Expr e2::Expr
{
  e1.env = e.env;
  e2.env = e.env ++ [pair(fst=name, snd=e1.value)];
  e.value = e2.value;
}

-- Functions

@{- Looks up a string in an environment and returns its associated value if it exists-}
function lookup
Value ::= s::String lookup_env::[Pair<String Value>]
{
  return
    if null(lookup_env) 
    then error(s ++ " does not exist")
      else 
      if head(lookup_env).fst == s
      then head(lookup_env).snd
        else lookup(s, tail(lookup_env));
}