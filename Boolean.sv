grammar evaluator;

abstract production boolLit
e::Expr ::= b::Boolean
{
    e.type = boolType();
    e.value = boolVal(b);

    e.newVarID = e.varID;
    e.newSubstMap = e.substMap;
}

-- Operators

abstract production eqOp
e::Expr ::= l::Expr r::Expr
{
    propagate env, typeEnv, typeErrors;

    e.value = boolVal(equalsValue(l.value, r.value));

    l.varID = e.varID;
    r.varID = l.newVarID;
    e.newVarID = r.newVarID;

    l.substMap = e.substMap;
    r.substMap = l.newSubstMap;
    e.newSubstMap = unify(l.type, r.type, r.newSubstMap);

    e.type = boolType();

    local newL :: Type = applySubstitution(l.type, e.newSubstMap);
    local newR :: Type = applySubstitution(r.type, e.newSubstMap);

    local unequalTypeError :: [String] = 
        if equalsType(^newL, ^newR)
        then []
        else ["Types do not match when comparing equality"];
    e.typeErrors <- unequalTypeError;
}

abstract production ifThenElse
e::Expr ::= b::Expr v1::Expr v2::Expr
{
    propagate env, typeEnv, typeErrors;

    e.value = case b.value of
    | boolVal(bVal) -> if bVal then v1.value else v2.value
    | _ -> error("Error evaluating if/then/else")
    end;

    b.varID = e.varID;
    v1.varID = b.newVarID;
    v2.varID = v1.newVarID;
    e.newVarID = v2.newVarID;

    b.substMap = e.substMap;
    v1.substMap = b.newSubstMap;
    v2.substMap = v1.newSubstMap;
    e.newSubstMap = unify(b.type, boolType(), unify(v1.type, v2.type, v2.newSubstMap));

    local newV1Type :: Type = applySubstitution(v1.type, e.newSubstMap);
    local newV2Type :: Type = applySubstitution(v2.type, e.newSubstMap);
    
    local isBoolError :: [String] = case b.type of
    | boolType() -> []
    | errType() -> []
    | _ -> ["Cannot evaluate if/then/else of a non-boolean statement"]
    end;

    local matchTypesError :: [String] = case ^newV1Type, ^newV2Type of
    | t1, t2 when equalsType(t1, t2) -> []
    | _, errType() -> []
    | errType(), _ -> []
    | _, _ -> ["Types evaluated in 'then' and 'else' do not match"]
    end;

    e.type = ^newV1Type;
    e.typeErrors <- isBoolError ++ matchTypesError;
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

concrete productions t::Term_c
| True { t.ast = boolLit(true); }
| False { t.ast = boolLit(false); }

concrete productions e::Expr_c
| Not e1::Expr_c { e.ast = notOp(e1.ast); }
| l::Expr_c EqOp r::Expr_c { e.ast = eqOp(l.ast, r.ast); }
| l::Expr_c And r::Expr_c { e.ast = andOp(l.ast, r.ast); }
| l::Expr_c Or r::Expr_c { e.ast = orOp(l.ast, r.ast); }

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
