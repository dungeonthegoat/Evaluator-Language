grammar evaluator;

abstract production boolLit
e::Expr ::= b::Boolean
{
  e.value = boolVal(b);
}

abstract production eqOp
eq_e::Expr ::= l::Expr r::Expr
{
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
  b.env = e.env ;
  v1.env = e.env ;
  v2.env = e.env ;
  
  e.value = case b.value of
    | boolVal(bVal) -> if bVal then v1.value else v2.value
    | _ -> error("Type error: attempted to check boolean value of non-boolean in an if/then/else statement")
  end;
}

abstract production andOp
e::Expr ::= l::Expr r::Expr
{
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
  b.env = e.env;
  
  e.value = case b.value of
    | boolVal(bVal) -> boolVal(!bVal)
    | _ -> error("Type error: 'not' used on non-boolean")
  end;
}

-- Concrete syntax

concrete production eqOp_c
e::Expr_c ::= e1::Expr_c '==' e2::Expr_c
{
    e.ast = eqOp(e1.ast, e2.ast);
}

concrete production ifThenElse_c
e::Expr_c ::= If b::Expr_c Then v1::Expr_c Else v2::Expr_c
{
    e.ast = ifThenElse(b.ast, v1.ast, v2.ast);
}

concrete production not_c
e::Expr_c ::= Not b::Expr_c
{
    e.ast = notOp(b.ast);
}

concrete production and_c
e::Expr_c ::= l::Expr_c And r::Expr_c
{
    e.ast = andOp(l.ast, r.ast);
}

concrete production or_c
e::Expr_c ::= l::Expr_c Or r::Expr_c
{
    e.ast = orOp(l.ast, r.ast);
}

-- Functions

@{- Gets the boolean value of a boolVal -}
function getBool
Boolean ::= v::Value
{
  return case v of
  | boolVal(b) -> b
  | _ -> error("Type error: expected a boolean")
  end;
}
