grammar evaluator;

-- Pattern match expression

abstract production matchExpr
e::Expr ::= target::Expr cases::MatchCases
{
  propagate env, typeEnv, typeErrors;

  cases.matchingVal = target.value;
  cases.matchingType = target.type;

  e.type = cases.type;
  e.value = cases.value;
}

-- MatchCase nonterminal

nonterminal MatchCase with env, pp, type, typeEnv, matchingVal, matchingType, isMatch, value, typeErrors;

abstract production matchCase
c::MatchCase ::= p::Pattern e::Expr
{
  propagate typeErrors;

  p.matchingVal = c.matchingVal;
  p.matchingType = c.matchingType;
  p.typeEnv = c.typeEnv;
  c.isMatch = p.isMatch;

  e.env = p.matchedVars ++ c.env;
  e.typeEnv = p.matchedVarTypes ++ c.typeEnv;

  c.value = e.value;
  c.type = e.type;
}

nonterminal MatchCases with env, pp, type, typeEnv, matchingVal, matchingType, isMatch, value, typeErrors;

abstract production emptyCase
c::MatchCases ::=
{
  c.isMatch = false;
  c.value = error("Empty match case");
  c.type = anyType();
  c.pp = "";
  c.typeErrors := [];
}

abstract production consCases
c::MatchCases ::= h::MatchCase t::MatchCases
{
  propagate env, typeEnv, matchingVal, matchingType, typeErrors;

  c.isMatch = h.isMatch || t.isMatch;

  c.type = h.type;
  c.value = if h.isMatch
            then h.value
            else t.value;
  c.typeErrors <- if equalsType(h.type, t.type)
                  then []
                  else ["Attempted to match pattern with return type " ++ toStringType(h.type) ++ " and " ++ toStringType(t.type)];
}

-- Concrete syntax

nonterminal MatchCase_c with ast_Match;
synthesized attribute ast_Match :: MatchCase;

concrete production matchCase_c
c::MatchCase_c ::= Bar p::Pat_c Arrow e::Expr_c
{
    c.ast_Match = matchCase(p.ast_p, e.ast);
}

nonterminal MatchCases_c with ast_Matches;
synthesized attribute ast_Matches :: MatchCases;

concrete production singleCase_c
cs::MatchCases_c ::= c::MatchCase_c
{
    cs.ast_Matches = consCases(c.ast_Match, emptyCase());
}

concrete production multiCase_c
cs::MatchCases_c ::= c::MatchCase_c rest::MatchCases_c
{
    cs.ast_Matches = consCases(c.ast_Match, rest.ast_Matches);
}

concrete production match_c
e::Expr_c ::= Case target::Expr_c LeftBrace cases::MatchCases_c RightBrace
{
    e.ast = matchExpr(target.ast, cases.ast_Matches);
}