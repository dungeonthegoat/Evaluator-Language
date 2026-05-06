grammar evaluator;

abstract production intLit
e::Expr ::= i::Integer
{
  e.type = intType();
  e.value = intVal(i);
}

abstract production floatLit
e::Expr ::= f::Float
{
  e.type = floatType();
  e.value = floatVal(f);
}

abstract production lessThanOp
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;

  e.type = boolType();
  e.typeErrors <- if numTypesMatch(l.type, r.type) then [] else ["Both operands of '<' must be numbers"];
  e.value = boolVal(getNum(l.value) < getNum(r.value));
}

abstract production greaterThanOp
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;

  e.type = boolType();
  e.typeErrors <- if numTypesMatch(l.type, r.type) then [] else ["Both operands of '>' must be numbers"];
  e.value = boolVal(getNum(l.value) > getNum(r.value));
}

abstract production lessThanEqOp
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;

  e.type = boolType();
  e.typeErrors <- if numTypesMatch(l.type, r.type) then [] else ["Both operands of '<=' must be numbers"];
  e.value = boolVal(getNum(l.value) <= getNum(r.value));
}

abstract production greaterThanEqOp
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;

  e.type = boolType();
  e.typeErrors <- if numTypesMatch(l.type, r.type) then [] else ["Both operands of '>=' must be numbers"];
  e.value = boolVal(getNum(l.value) >= getNum(r.value));
}

abstract production negOp
e::Expr ::= e1::Expr
{
  propagate env, typeEnv, typeErrors;

  local typeMatchError :: [String] =
    case e1.type of
    | intType() -> []
    | floatType() -> []
    | errType() -> []
    | _ -> ["Unable to negate non-number"]
    end;
  
  e.type = 
    case e1.type of
    | intType() -> intType()
    | floatType() -> floatType()
    | _ -> errType()
    end;
  e.typeErrors <- typeMatchError;
  e.value =
    case e1.value of
    | intVal(i) -> intVal(-i)
    | floatVal(f) -> floatVal(-f)
    | _ -> emptyListVal()
    end;
}

abstract production addOp
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;

  e.typeErrors <- if numTypesMatch(l.type, r.type) then [] else ["Both operands of '+' must be numbers"];
  e.type = getBinaryArithmeticType(l.type, r.type);
  e.value = 
    case l.value, r.value of
    | intVal(i1), intVal(i2) -> intVal(i1 + i2)
    | floatVal(f1), floatVal(f2) -> floatVal(f1 + f2)
    | floatVal(f1), intVal(i2) -> floatVal(f1 + toFloat(i2))
    | intVal(i1), floatVal(f2) -> floatVal(toFloat(i1) + f2)
    | _, _ -> emptyListVal()
    end;
}

abstract production subOp
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;

  e.typeErrors <- if numTypesMatch(l.type, r.type) then [] else ["Both operands of '-' must be numbers"];
  e.type = getBinaryArithmeticType(l.type, r.type);
  e.value = 
    case l.value, r.value of
    | intVal(i1), intVal(i2) -> intVal(i1 - i2)
    | floatVal(f1), floatVal(f2) -> floatVal(f1 - f2)
    | floatVal(f1), intVal(i2) -> floatVal(f1 - toFloat(i2))
    | intVal(i1), floatVal(f2) -> floatVal(toFloat(i1) - f2)
    | _, _ -> emptyListVal()
    end;
}

abstract production mulOp
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;

  e.typeErrors <- if numTypesMatch(l.type, r.type) then [] else ["Both operands of '*' must be numbers"];
  e.type = getBinaryArithmeticType(l.type, r.type);
  e.value = 
    case l.value, r.value of
    | intVal(i1), intVal(i2) -> intVal(i1 * i2)
    | floatVal(f1), floatVal(f2) -> floatVal(f1 * f2)
    | floatVal(f1), intVal(i2) -> floatVal(f1 * toFloat(i2))
    | intVal(i1), floatVal(f2) -> floatVal(toFloat(i1) * f2)
    | _, _ -> emptyListVal()
    end;
}

abstract production divOp
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;

  local typeMatchError :: [String] = if numTypesMatch(l.type, r.type) then [] else ["Both operands of '/' must be numbers"];
  local divByZeroError :: [String] = case r.value of
    | intVal(0) -> ["Cannot divide by zero"]
    | _ -> []
  end;
  e.typeErrors <- typeMatchError ++ divByZeroError;
  e.type = getBinaryArithmeticType(l.type, r.type);
  e.value = 
    case l.value, r.value of
    | intVal(i1), intVal(i2) -> intVal(i1 / i2)
    | floatVal(f1), floatVal(f2) -> floatVal(f1 / f2)
    | floatVal(f1), intVal(i2) -> floatVal(f1 / toFloat(i2))
    | intVal(i1), floatVal(f2) -> floatVal(toFloat(i1) / f2)
    | _, _ -> emptyListVal()
    end;
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

concrete productions t::Term_c
| i::IntLitT { t.ast = intLit(toInteger(i.lexeme)); }
| f::FloatLitT { t.ast = floatLit(toFloat(f.lexeme)); }

concrete productions e::Expr_c
| Dash e1::Expr_c { e.ast = negOp(e1.ast); }
| l::Expr_c Plus r::Expr_c { e.ast = addOp(l.ast, r.ast); }
| l::Expr_c Dash r::Expr_c { e.ast = subOp(l.ast, r.ast); }
| l::Expr_c Star r::Expr_c { e.ast = mulOp(l.ast, r.ast); }
| l::Expr_c Slash r::Expr_c { e.ast = divOp(l.ast, r.ast); }
| l::Expr_c Modulo r::Expr_c { e.ast = modOp(l.ast, r.ast); }
| l::Expr_c Pow r::Expr_c { e.ast = powOp(l.ast, r.ast); }
| l::Expr_c Less r::Expr_c { e.ast = lessThanOp(l.ast, r.ast); }
| l::Expr_c Greater r::Expr_c { e.ast = greaterThanOp(l.ast, r.ast); }
| l::Expr_c LessEq r::Expr_c { e.ast = lessThanEqOp(l.ast, r.ast); }
| l::Expr_c GreaterEq r::Expr_c { e.ast = greaterThanEqOp(l.ast, r.ast); }

-- Functions

@{- Gets the integer value of an intVal -}
function getInt
Integer ::= v::Value
{
  return case v of
  | intVal(i) -> i
  | _ -> error("Unexpected error fetching integer value")
  end;
}

@{- Gets the float value of a floatVal -}
function getFloat
Float ::= v::Value
{
  return case v of
  | floatVal(f) -> f
  | _ -> error("Unexpected error fetching float value")
  end;
}

@{- Gets the float value of any number-representing value -}
function getNum
Float ::= v::Value
{
  return case v of
  | floatVal(f) -> f
  | intVal(i) -> toFloat(i)
  | _ -> error("Unexpected error fetching number value")
  end;
}

function getBinaryArithmeticType
Type ::= l::Type r::Type
{
  return case l, r of
  | intType(), intType() -> intType()
  | floatType(), floatType() -> floatType()
  | floatType(), intType() -> floatType()
  | intType(), floatType() -> floatType()
  | _, _ -> errType()
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

@{- Determines whether two types should result in a type error during number operations -}
function numTypesMatch
Boolean ::= t1::Type t2::Type
{
  return case t1, t2 of
  | intType(), intType() -> true
  | floatType(), floatType() -> true
  | floatType(), intType() -> true
  | intType(), floatType() -> true
  | errType(), _ -> true
  | _, errType() -> true
  | _, _ -> false
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