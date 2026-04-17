grammar evaluator;

abstract production numLit
e::Expr ::= i::Integer
{
  e.value = intVal(i);
}

-- Comparisons

abstract production lessThanOp
e::Expr ::= l::Expr r::Expr
{
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

-- Operators

abstract production addOp
sum::Expr ::= l::Expr r::Expr
{
  l.env = sum.env ;
  r.env = sum.env ;
  sum.value = intVal(getInt(l.value) + getInt(r.value)) ;
}

abstract production subOp
dff::Expr ::= l::Expr r::Expr
{
  l.env = dff.env ;
  r.env = dff.env ;
  dff.value = intVal(getInt(l.value) - getInt(r.value)) ;
}

abstract production mulOp
mul::Expr ::= l::Expr r::Expr
{
  l.env = mul.env ;
  r.env = mul.env ;
  mul.value = intVal(getInt(l.value) * getInt(r.value)) ;
}

abstract production divOp
div::Expr ::= l::Expr r::Expr
{
  l.env = div.env ;
  r.env = div.env ;
  div.value = intVal(getInt(l.value) / getInt(r.value)) ;
}

abstract production modOp
e::Expr ::= l::Expr r::Expr
{
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

-- Concrete syntax

concrete production add_c
add_e::Expr_c ::= e1::Expr_c '+' e2::Expr_c
{
    add_e.ast = addOp(e1.ast, e2.ast);
}

concrete production sub_c
sub_e::Expr_c ::= e1::Expr_c '-' e2::Expr_c
{
    sub_e.ast = subOp(e1.ast, e2.ast);
}

concrete production mul_c
mul_e::Expr_c ::= e1::Expr_c '*' e2::Expr_c
{
    mul_e.ast = mulOp(e1.ast, e2.ast);
}

concrete production div_c
div_e::Expr_c ::= e1::Expr_c '/' e2::Expr_c
{
    div_e.ast = divOp(e1.ast, e2.ast);
}

concrete production mod_c
e::Expr_c ::= l::Expr_c Modulo r::Expr_c
{
    e.ast = modOp(l.ast, r.ast);
}

-- Comparisons

concrete production lessThan_c
e::Expr_c ::= l::Expr_c Less r::Expr_c
{
    e.ast = lessThanOp(l.ast, r.ast);
}

concrete production greaterThan_c
e::Expr_c ::= l::Expr_c Greater r::Expr_c
{
    e.ast = greaterThanOp(l.ast, r.ast);
}

concrete production lessThanEq_c
e::Expr_c ::= l::Expr_c LessEq r::Expr_c
{
    e.ast = lessThanEqOp(l.ast, r.ast);
}

concrete production greaterThanEq_c
e::Expr_c ::= l::Expr_c GreaterEq r::Expr_c
{
    e.ast = greaterThanEqOp(l.ast, r.ast);
}

-- Functions

@{- Gets the integer value of an intVal -}
function getInt
Integer ::= v::Value
{
  return case v of
  | intVal(i) -> i
  | _ -> error("Type error: expected an integer")
  end;
}