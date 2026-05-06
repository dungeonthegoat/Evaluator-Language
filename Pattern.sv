grammar evaluator;

nonterminal Pattern with pp, matchingVal, matchingType, isMatch, matchedVars, matchedVarTypes, typeErrors, typeEnv;

inherited attribute matchingVal :: Value;
inherited attribute matchingType :: Type;
synthesized attribute isMatch :: Boolean;
synthesized attribute matchedVars :: [Pair<String Value>];
synthesized attribute matchedVarTypes :: [Pair<String Type>];

-- Productions

aspect default production
p::Pattern ::=
{
  p.typeErrors := [];
} 

abstract production emptyListPat
p::Pattern ::=
{
  p.isMatch = case p.matchingVal of
  | emptyListVal() -> true
  | _ -> false
  end;

  p.matchedVars = [];
  p.matchedVarTypes = [];
  p.typeErrors := case p.matchingType of
  | listType(_) -> []
  | anyType() -> []
  | errType() -> []
  | _ -> ["Cannot match a non-list with pattern []"]
  end;
}

abstract production tuplePat
p::Pattern ::= l::Pattern r::Pattern
{
  propagate typeErrors, typeEnv;

  local typeMatchError :: [String] = 
    case p.matchingType of
    | tupleType(_, _) -> []
    | anyType() -> []
    | errType() -> []
    | _ -> ["Cannot match a non-tuple with pattern (l, r)"]
    end;
  
  local isTuple :: Boolean = case p.matchingVal of
  | tupleVal(_, _) -> true
  | _ -> false
  end;

  l.matchingType = case p.matchingType of
  | tupleType(lt, _) -> ^lt
  | _ -> errType()
  end;

  r.matchingType = case p.matchingType of
  | tupleType(_, rt) -> ^rt
  | _ -> errType()
  end;
  
  l.matchingVal = case p.matchingVal of
  | tupleVal(l, _) -> ^l
  | _ -> emptyListVal()
  end;

  r.matchingVal = case p.matchingVal of
  | tupleVal(_, r) -> ^r
  | _ -> emptyListVal()
  end;

  p.isMatch = isTuple && l.isMatch && r.isMatch;

  p.matchedVars = if isTuple then l.matchedVars ++ r.matchedVars else [];
  p.matchedVarTypes = if null(typeMatchError) then l.matchedVarTypes ++ r.matchedVarTypes else [];
  p.typeErrors <- typeMatchError;
}

abstract production consPat
p::Pattern ::= p1::Pattern p2::Pattern
{
  propagate typeErrors, typeEnv;

  local typeMatchError :: [String] = 
    case p.matchingType of
    | listType(_) -> []
    | anyType() -> []
    | errType() -> []
    | _ -> ["Cannot match a non-list with pattern h::t"]
    end;

  local isCons :: Boolean = case p.matchingVal of
  | consVal(_, _) -> true
  | _ -> false
  end;

  p1.matchingType = case p.matchingType of
  | listType(t) -> ^t
  | _ -> errType()
  end;

  p2.matchingType = case p.matchingType of
  | listType(t) -> listType(^t)
  | _ -> errType()
  end;
  
  p1.matchingVal = case p.matchingVal of
  | consVal(h, _) -> ^h
  | _ -> emptyListVal()
  end;

  p2.matchingVal = case p.matchingVal of
  | consVal(_, t) -> ^t
  | _ -> emptyListVal()
  end;

  p.isMatch = isCons && p1.isMatch && p2.isMatch;

  p.matchedVars = if isCons then p1.matchedVars ++ p2.matchedVars else [];
  p.matchedVarTypes = if null(typeMatchError) then p1.matchedVarTypes ++ p2.matchedVarTypes else [];
  p.typeErrors <- typeMatchError;
}

nonterminal PatternArgs with isMatch, matchedVars, matchedVarTypes, typeErrors;
inherited attribute matchingVals :: [Value];

abstract production emptyCustomPatArgs
pa::PatternArgs ::=
{

}

abstract production customPat
p::Pattern ::= typeName::String ps::[Pattern]
{
  propagate typeErrors;

  local constrType :: Type = lookupType(typeName, p.typeEnv);
  local argTypes :: [Type] = getConstrArgTypes(^constrType);
  local retType :: Type = getConstrRetType(^constrType);

  local typeMatchError :: [String] = 
    case p.matchingType, retType of
    | _, errType() -> ["Undefined constructor type " ++ typeName]
    | t1, t2 when equalsType(t1, ^t2) -> []
    | anyType(), _ -> []
    | errType(), _ -> []
    | _, _ -> ["Pattern of type " ++ typeName ++ " does not match " ++ toStringType(p.matchingType)]
    end;
  
  local isCorrectCustom :: Boolean =
    case p.matchingVal of
    | customVal(n, _) when n == typeName -> true
    | _ -> false
    end;
  
  local vals :: [Value] =
    case p.matchingVal of
    | customVal(_, vs) -> vs
    | _ -> []
    end;
  
  local decoratedPats :: [Decorated Pattern] = decoratePatternsWithValues(ps, vals, p.typeEnv);
  local decoratedTypePats :: [Decorated Pattern] = decoratePatternsWithTypes(ps, argTypes, p.typeEnv);

  local allMatch :: Boolean =
    length(ps) == length(vals) && -- If lengths don't match, then the pattern clearly does not match
    foldr(\b1::Boolean b2::Boolean -> b1 && b2, true, map(\dec::Decorated Pattern -> dec.isMatch, decoratedPats));
  
  p.isMatch = isCorrectCustom && allMatch;

  p.matchedVars = foldr(append, [], map(\dec::Decorated Pattern -> dec.matchedVars, decoratedPats));
  p.matchedVarTypes = foldr(append, [], map(\dec::Decorated Pattern -> dec.matchedVarTypes, decoratedTypePats));

  p.typeErrors <- typeMatchError ++ foldr(append, [], map(\dec::Decorated Pattern -> dec.typeErrors, decoratedTypePats));
}

abstract production varPat
p::Pattern ::= name::String
{
  p.isMatch = true;
  p.matchedVars = [pair(fst = name, snd = p.matchingVal)];
  p.matchedVarTypes = [pair(fst = name, snd = p.matchingType)];
}

-- Concrete syntax

nonterminal Pat_c with ast_p;
synthesized attribute ast_p :: Pattern;

concrete productions p::Pat_c
| '[' ']' { p.ast_p = emptyListPat(); }
| h::Pat_c '::' t::Pat_c { p.ast_p = consPat(h.ast_p, t.ast_p); }
| v::Var { p.ast_p = varPat(v.lexeme); }
| '(' l::Pat_c ',' r::Pat_c ')' { p.ast_p = tuplePat(l.ast_p, r.ast_p); }
| v::TypeName '(' ps::PatList_c ')' { p.ast_p = customPat(v.lexeme, ps.pats); }

nonterminal PatList_c with pats;
synthesized attribute pats :: [Pattern];

concrete productions ps::PatList_c
| p::Pat_c { ps.pats = [p.ast_p]; }
| p::Pat_c Sep rest::PatList_c { ps.pats = p.ast_p :: rest.pats; }


@{- Decorates a list of patterns with a list of values -}
function decoratePatternsWithValues
[Decorated Pattern] ::= pats::[Pattern] vals::[Value] env::[(String, Type)]
{
  return case pats, vals of
  | [], [] -> []
  | p::ps, v::vs ->
    let
      decorated :: Decorated Pattern = decorate p with {
        matchingVal = v;
        matchingType = anyType();
        typeEnv = env;
      }
    in
      decorated :: decoratePatternsWithValues(ps, vs, env)
    end
  | _, _ -> []
  end;
}

@{- Decorates a list of patterns with generic anyTypes -}
function decoratePatternsWithTypes
[Decorated Pattern] ::= pats::[Pattern] types::[Type] env::[(String, Type)]
{
  return case pats, types of
  | [], [] -> []
  | p::ps, t::ts ->
    let
      decorated :: Decorated Pattern = decorate p with {
        matchingVal = emptyListVal();
        matchingType = t;
        typeEnv = env;
      }
    in
      decorated :: decoratePatternsWithTypes(ps, ts, env)
    end
  | _, _ -> []
  end;
}

function getConstrArgTypes
[Type] ::= constrType::Type
{
  return case constrType of
  | funcType(arg_t, ret_t) -> ^arg_t :: getConstrArgTypes(^ret_t)
  | _ -> []
  end;
}

function getConstrRetType
Type ::= constrType::Type
{
  return case constrType of
  | funcType(_, ret_t) -> getConstrRetType(^ret_t)
  | _ -> ^constrType
  end;
}