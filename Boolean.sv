grammar evaluator;

abstract production boolLit
e::Expr ::= b::Boolean
{
  e.type = boolType();
  e.value = boolVal(b);
}

-- Operators

abstract production eqOp
eq_e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;

  local unequalTypeError :: [String] = 
    if equalsType(l.type, r.type)
    then []
    else ["Types do not match when comparing equality"];
  
  eq_e.typeErrors <- unequalTypeError;
  eq_e.type = boolType();
  eq_e.value = boolVal(equalsValue(l.value, r.value));
}

abstract production ifThenElse
e::Expr ::= b::Expr v1::Expr v2::Expr
{
  propagate env, typeEnv, typeErrors;
  
  local isBoolError :: [String] = case b.type of
  | boolType() -> []
  | errType() -> []
  | _ -> ["Cannot evaluate if/then/else of a non-boolean statement"]
  end;

  local matchTypesError :: [String] = case v1.type, v2.type of
  | t1, t2 when equalsType(t1, t2) -> []
  | _, errType() -> []
  | errType(), _ -> []
  | _, _ -> ["Types evaluated in 'then' and 'else' do not match"]
  end;

  e.type = v1.type;
  e.typeErrors <- isBoolError ++ matchTypesError;
  e.value = case b.value of
  | boolVal(bVal) -> if bVal then v1.value else v2.value
  | _ -> error("Error evaluating if/then/else")
  end;
}

abstract production andOp
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;
  
  local leftErrors :: [String] = case l.type of
  | boolType() -> []
  | errType() -> []
  | _ -> ["Left operand of 'and' must be a boolean"]
  end;

  local rightErrors :: [String] = case r.type of
  | boolType() -> []
  | errType() -> []
  | _ -> ["Right operand of 'and' must be a boolean"]
  end;

  e.type = case l.type, r.type of
  | boolType(), boolType() -> boolType()
  | _, _ -> errType()
  end;

  e.typeErrors <- leftErrors ++ rightErrors;
  e.value = boolVal(getBool(l.value) && getBool(r.value));
}

abstract production orOp
e::Expr ::= l::Expr r::Expr
{
  propagate env, typeEnv, typeErrors;
  
  local leftErrors :: [String] = case l.type of
  | boolType() -> []
  | errType() -> []
  | _ -> ["Left operand of 'or' must be a boolean"]
  end;

  local rightErrors :: [String] = case r.type of
  | boolType() -> []
  | errType() -> []
  | _ -> ["Right operand of 'or' must be a boolean"]
  end;

  e.type = case l.type, r.type of
  | boolType(), boolType() -> boolType()
  | _, _ -> errType()
  end;

  e.typeErrors <- leftErrors ++ rightErrors;

  e.value = boolVal(getBool(l.value) || getBool(r.value));
}

abstract production notOp
e::Expr ::= b::Expr
{
  propagate env, typeEnv, typeErrors;

  local notBoolError :: [String] = case b.type of
  | boolType() -> []
  | errType() -> []
  | _ -> ["Operand of 'not' must be a boolean"]
  end;

  e.type = case b.type of
  | boolType() -> boolType()
  | _ -> errType()
  end;
  e.typeErrors <- notBoolError;
  e.value = boolVal(!getBool(b.value));
}

-- Concrete syntax

concrete production true_c
e::Expr_c ::= True
{
  e.ast = boolLit(true);
}

concrete production false_c
e::Expr_c ::= False
{
  e.ast = boolLit(false);
}

concrete production eqOp_c
e::Expr_c ::= e1::Expr_c EqOp e2::Expr_c
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
  | _ -> error("Error fetching boolean value")
  end;
}
