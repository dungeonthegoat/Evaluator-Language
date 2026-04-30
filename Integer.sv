grammar evaluator;

abstract production numLit
e::Expr ::= i::Integer
{
  e.type = intType();
  e.value = intVal(i);
}

abstract production lessThanOp
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;

  e.type = boolType();
  e.typeErrors <- if intTypesMatch(l.type, r.type) then [] else ["Both operands of '<' must be integers"];
  e.value = boolVal(getInt(l.value) < getInt(r.value));
}

abstract production greaterThanOp
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;

  e.type = boolType();
  e.typeErrors <- if intTypesMatch(l.type, r.type) then [] else ["Both operands of '>' must be integers"];
  e.value = boolVal(getInt(l.value) > getInt(r.value));
}

abstract production lessThanEqOp
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;

  e.type = boolType();
  e.typeErrors <- if intTypesMatch(l.type, r.type) then [] else ["Both operands of '<=' must be integers"];
  e.value = boolVal(getInt(l.value) <= getInt(r.value));
}

abstract production greaterThanEqOp
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;

  e.type = boolType();
  e.typeErrors <- if intTypesMatch(l.type, r.type) then [] else ["Both operands of '>=' must be integers"];
  e.value = boolVal(getInt(l.value) >= getInt(r.value));
}

abstract production addOp
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;

  e.typeErrors <- if intTypesMatch(l.type, r.type) then [] else ["Both operands of '+' must be integers"];
  e.type = intType();
  e.value = intVal(getInt(l.value) + getInt(r.value));
}

abstract production subOp
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;

  e.typeErrors <- if intTypesMatch(l.type, r.type) then [] else ["Both operands of '-' must be integers"];
  e.type = intType();
  e.value = intVal(getInt(l.value) - getInt(r.value));
}

abstract production mulOp
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;

  e.typeErrors <- if intTypesMatch(l.type, r.type) then [] else ["Both operands of '*' must be integers"];
  e.type = intType();
  e.value = intVal(getInt(l.value) * getInt(r.value));
}

abstract production divOp
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;

  local typeMatchError :: [String] = if intTypesMatch(l.type, r.type) then [] else ["Both operands of '/' must be integers"];
  local divByZeroError :: [String] = case r.value of
    | intVal(0) -> ["Cannot divide by zero"]
    | _ -> []
  end;
  e.typeErrors <- typeMatchError ++ divByZeroError;
  e.type = intType();
  e.value = intVal(getInt(l.value) / getInt(r.value)) ;
}

abstract production powOp
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;

  e.typeErrors <- if intTypesMatch(l.type, r.type) then [] else ["Both operands of '^' must be integers"];
  e.type = intType();
  e.value = intVal(pow(getInt(l.value), getInt(r.value)));
}

abstract production modOp
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;

  e.typeErrors <- if intTypesMatch(l.type, r.type) then [] else ["Both operands of '%' must be integers"];
  e.type = intType();
  e.value = intVal(getInt(l.value) - (getInt(l.value) / getInt(r.value)) * getInt(r.value));
}

-- Concrete syntax

concrete productions e::Expr_c
| i::IntLit { e.ast = numLit(toInteger(i.lexeme)); }
| l::Expr_c '+' r::Expr_c { e.ast = addOp(l.ast, r.ast); }
| l::Expr_c '-' r::Expr_c { e.ast = subOp(l.ast, r.ast); }
| l::Expr_c '*' r::Expr_c { e.ast = mulOp(l.ast, r.ast); }
| l::Expr_c '/' r::Expr_c { e.ast = divOp(l.ast, r.ast); }
| l::Expr_c '%' r::Expr_c { e.ast = modOp(l.ast, r.ast); }
| l::Expr_c '^' r::Expr_c { e.ast = powOp(l.ast, r.ast); }
| l::Expr_c '<' r::Expr_c { e.ast = lessThanOp(l.ast, r.ast); }
| l::Expr_c '>' r::Expr_c { e.ast = greaterThanOp(l.ast, r.ast); }
| l::Expr_c '<=' r::Expr_c { e.ast = lessThanEqOp(l.ast, r.ast); }
| l::Expr_c '>=' r::Expr_c { e.ast = greaterThanEqOp(l.ast, r.ast); }

-- Helpful Functions

@{- Gets the integer value of an intVal -}
function getInt
Integer ::= v::Value
{
  return case v of
  | intVal(i) -> i
  | _ -> error("Unexpected error fetching integer value")
  end;
}

@{- Returns x^y, assuming y >= 0 -}
function pow
Integer ::= x::Integer y::Integer
{
  return case y of
  | y when y <= 0 -> 1
  | _ -> x * pow(x, y - 1)
  end;
}

@{- Determines whether two types should result in a type error during integer operations -}
function intTypesMatch
Boolean ::= t1::Type t2::Type
{
  return case t1, t2 of
  | intType(), intType() -> true
  | errType(), _ -> true
  | _, errType() -> true
  | _, _ -> false
  end;
}