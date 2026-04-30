grammar evaluator;

nonterminal Pattern with pp, matchingVal, matchingType, isMatch, matchedVars, matchedVarTypes, typeErrors;

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
  | _ -> ["Cannot match a non-list with pattern []"]
  end;
}

abstract production tuplePat
p::Pattern ::= l::Pattern r::Pattern
{
  propagate typeErrors;

  local typeMatchError :: [String] = 
    case p.matchingType of
    | tupleType(_, _) -> []
    | anyType() -> []
    | _ -> ["Cannot match a non-list with pattern h::t"]
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
  propagate typeErrors;

  local typeMatchError :: [String] = 
    case p.matchingType of
    | listType(_) -> []
    | anyType() -> []
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