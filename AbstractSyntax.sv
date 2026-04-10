grammar evaluator;

-- Attributes
synthesized attribute value :: Value;
inherited attribute env :: [Pair<String Value>];
synthesized attribute ast_rep :: Expr;

-- Nonterminals
nonterminal Value;
nonterminal Root with value;
nonterminal Expr with value, env, ast_rep;

-- Root Production

abstract production root
r::Root ::= e::Expr
{
  e.env = [];
  r.value = e.value ;
}

-- Value Definitions

abstract production intVal
v::Value ::= i::Integer
{}

abstract production closureVal
v::Value ::= param::String body::Expr env::[Pair<String Value>]
{}

abstract production boolVal
v::Value ::= b::Boolean
{}

abstract production recClosureVal
v::Value ::= recName::String param::String body::Expr env::[Pair<String Value>]
{}

-- Operations

abstract production numLit
e::Expr ::= i::Integer
{
  e.ast_rep = numLit(i);
  e.value = intVal(i);
}

abstract production boolLit
e::Expr ::= b::Boolean
{
  e.ast_rep = boolLit(b);
  e.value = boolVal(b);
}

abstract production var
e::Expr ::= name::String
{
  e.ast_rep = var(name);
  e.value = lookup(name, e.env);
}

abstract production eqOp
eq_e::Expr ::= l::Expr r::Expr
{
  eq_e.ast_rep = eqOp(l.ast_rep, r.ast_rep);
  l.env = eq_e.env ;
  r.env = eq_e.env ;
  eq_e.value = case l.value of
    | intVal(lInt) ->
      case r.value of
        | intVal(rInt) -> boolVal(lInt == rInt)
        | _ -> error("Type error: attempted to compare an integer with a non-integer")
      end
    
    | boolVal(lBool) ->
      case r.value of
        | boolVal(rBool) -> boolVal(lBool == rBool)
        | _ -> error("Type error: attempted to compare a boolean with a non-boolean")
      end
    
    | _ -> error("Type error: failure comparing")
  end;
}

abstract production ifThenElse
e::Expr ::= b::Expr v1::Expr v2::Expr
{
  e.ast_rep = ifThenElse(b.ast_rep, v1.ast_rep, v2.ast_rep);
  b.env = e.env ;
  v1.env = e.env ;
  v2.env = e.env ;

  e.value = case b.value of
    | boolVal(bVal) -> if bVal then v1.value else v2.value
    | _ -> error("Type error: attempted to check boolean value of non-boolean in an if/then/else statement")
  end;
}

abstract production lessThanOp
e::Expr ::= l::Expr r::Expr
{
  e.ast_rep = lessThanOp(l.ast_rep, r.ast_rep);
  l.env = e.env ;
  r.env = e.env ;
  
  e.value = case l.value of
    | intVal(lInt) ->
      case r.value of
        | intVal(rInt) -> boolVal(lInt < rInt)
        | _ -> error("Type error: attempted to compare (<) an integer and a non-integer")
      end
    | _ -> error("Type error: attempted to compare (<) with a non-integer")
  end;
}

abstract production greaterThanOp
e::Expr ::= l::Expr r::Expr
{
  e.ast_rep = greaterThanOp(l.ast_rep, r.ast_rep);
  l.env = e.env ;
  r.env = e.env ;
  
  e.value = case l.value of
    | intVal(lInt) ->
      case r.value of
        | intVal(rInt) -> boolVal(lInt > rInt)
        | _ -> error("Type error: attempted to compare (>) an integer and a non-integer")
      end
    | _ -> error("Type error: attempted to compare (>) with a non-integer")
  end;
}

abstract production lessThanEqOp
e::Expr ::= l::Expr r::Expr
{
  e.ast_rep = lessThanEqOp(l.ast_rep, r.ast_rep);
  l.env = e.env ;
  r.env = e.env ;
  
  e.value = case l.value of
    | intVal(lInt) ->
      case r.value of
        | intVal(rInt) -> boolVal(lInt <= rInt)
        | _ -> error("Type error: attempted to compare (<=) an integer and a non-integer")
      end
    | _ -> error("Type error: attempted to compare (<=) with a non-integer")
  end;
}

abstract production greaterThanEqOp
e::Expr ::= l::Expr r::Expr
{
  e.ast_rep = greaterThanEqOp(l.ast_rep, r.ast_rep);
  l.env = e.env ;
  r.env = e.env ;
  
  e.value = case l.value of
    | intVal(lInt) ->
      case r.value of
        | intVal(rInt) -> boolVal(lInt >= rInt)
        | _ -> error("Type error: attempted to compare (>=) an integer and a non-integer")
      end
    | _ -> error("Type error: attempted to compare (>=) with a non-integer")
  end;
}

abstract production andOp
e::Expr ::= l::Expr r::Expr
{
  e.ast_rep = andOp(l.ast_rep, r.ast_rep);
  l.env = e.env;
  r.env = e.env;
  e.value = case l.value of
    | boolVal(lBool) ->
      case r.value of
        | boolVal(rBool) -> boolVal(lBool && rBool)
        | _ -> error("Type error: 'and' used on non-boolean")
      end
    | _ -> error("Type error: 'and' used on non-boolean")
  end;
}

abstract production orOp
e::Expr ::= l::Expr r::Expr
{
  e.ast_rep = orOp(l.ast_rep, r.ast_rep);
  l.env = e.env;
  r.env = e.env;
  e.value = case l.value of
    | boolVal(lBool) ->
      case r.value of
        | boolVal(rBool) -> boolVal(lBool || rBool)
        | _ -> error("Type error: 'or' used on non-boolean")
      end
    | _ -> error("Type error: 'or' used on non-boolean")
  end;
}

abstract production notOp
e::Expr ::= b::Expr
{
  e.ast_rep = notOp(b.ast_rep);
  b.env = e.env;
  e.value = case b.value of
    | boolVal(bVal) -> boolVal(!bVal)
    | _ -> error("Type error: 'not' used on non-boolean")
  end;
}

abstract production addOp
sum::Expr ::= l::Expr r::Expr
{
  sum.ast_rep = addOp(l.ast_rep, r.ast_rep);
  l.env = sum.env ;
  r.env = sum.env ;
  sum.value = intVal(getInt(l.value) + getInt(r.value)) ;
}

abstract production subOp
dff::Expr ::= l::Expr r::Expr
{
  dff.ast_rep = subOp(l.ast_rep, r.ast_rep);
  l.env = dff.env ;
  r.env = dff.env ;
  dff.value = intVal(getInt(l.value) - getInt(r.value)) ;
}

abstract production mulOp
mul::Expr ::= l::Expr r::Expr
{
  mul.ast_rep = mulOp(l.ast_rep, r.ast_rep);
  l.env = mul.env ;
  r.env = mul.env ;
  mul.value = intVal(getInt(l.value) * getInt(r.value)) ;
}

abstract production divOp
div::Expr ::= l::Expr r::Expr
{
  div.ast_rep = divOp(l.ast_rep, r.ast_rep);
  l.env = div.env ;
  r.env = div.env ;
  div.value = intVal(getInt(l.value) / getInt(r.value)) ;
}

abstract production modOp
e::Expr ::= l::Expr r::Expr
{
  e.ast_rep = modOp(l.ast_rep, r.ast_rep);
  l.env = e.env ;
  r.env = e.env ;
  e.value = case l.value of
    | intVal(lInt) -> case r.value of
      | intVal(rInt) -> intVal(lInt - (lInt / rInt) * rInt)
      | _ -> error("Type error: modulo of non-integer")
      end
    | _ -> error("Type error: modulo of non-integer")
  end;
}

abstract production letExpr
e::Expr ::= name::String e1::Expr e2::Expr
{
  e.ast_rep = letExpr(name, e1.ast_rep, e2.ast_rep);
  e1.env = e.env ;
  e2.env = e.env ++ [pair(fst=name, snd=e1.value)] ;
  e.value = e2.value ;
}

abstract production letRecExpr
e::Expr ::= name::String param::String body::Expr e2::Expr
{
  e.ast_rep = letRecExpr(name, param, body.ast_rep, e2.ast_rep);

  e2.env = e.env ++ [pair(fst=name, snd=recClosureVal(name, param, body.ast_rep, e.env))] ;
  body.env = e.env ;

  e.value = e2.value ;
}

abstract production lambda
e::Expr ::= param::String b::Expr
{
  e.ast_rep = lambda(param, b.ast_rep);
  e.value = closureVal(param, b.ast_rep, e.env);
  b.env = e.env;
}

abstract production app
e::Expr ::= func::Expr arg::Expr
{
  e.ast_rep = app(func.ast_rep, arg.ast_rep);
  
  arg.env = e.env;
  func.env = e.env;

  -- Get parameter name
  local attribute param :: String ;
  param = case func.value of
  | closureVal(p, _, _) -> p
  | recClosureVal(_, p, _, _) -> p
  | _ -> error("Type error: expected a function")
  end;

  -- Get body
  local attribute body :: Expr ;
  body = case func.value of
  | closureVal(_, b, _) -> b.ast_rep
  | recClosureVal(_, _, b, _) -> b.ast_rep
  | _ -> error("Type error: expected a function")
  end;

  -- Get environment
  local attribute f_env :: [Pair<String Value>] ;
  f_env = case func.value of
  | closureVal(_, _, f) -> f
  | recClosureVal(name, _, _, f) -> f ++ [pair(fst=name, snd=func.value)] -- Insert recursive function into its own environment
  | _ -> []
  end;

  -- Decorate the body tree
  body.env = f_env ++ [pair(fst=param, snd=arg.value)] ;
  
  e.value = body.value;
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

@{- Gets the integer value of an intVal -}
function getInt
Integer ::= v::Value
{
  return case v of
  | intVal(i) -> i
  | _ -> error("Type error: expected an integer")
  end;
}

@{- Gets the boolean value of a boolVal -}
function getBool
Boolean ::= v::Value
{
  return case v of
  | boolVal(b) -> b
  | _ -> error("Type error: expected a boolean")
  end;
}

@{- Converts a Value to a string -}
function toStringValue
String ::= v::Value
{
  return case v of
  | intVal(i) -> toString(i)
  | closureVal(p, _, _) -> "Closure of " ++ p
  | recClosureVal(f, p, _, _) -> "Recursive closure " ++ f ++ " of " ++ p
  | boolVal(b) -> toString(b)
  end;
}