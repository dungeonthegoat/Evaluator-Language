grammar evaluator;

nonterminal MatchCase with env, matchingVal, isMatch, value;

abstract production matchCase
c::MatchCase ::= p::Pattern e::Expr
{
    p.matchingVal = c.matchingVal;
    c.isMatch = p.isMatch;

    e.env = c.env ++ p.matchedVars;
    c.value = e.value;
}

nonterminal MatchCases with env, matchingVal, isMatch, value;

abstract production emptyCase
c::MatchCases ::=
{
    c.isMatch = false;
    c.value = error("Empty match case");
}

abstract production consCases
c::MatchCases ::= h::MatchCase t::MatchCases
{
    h.env = c.env;
    h.matchingVal = c.matchingVal;
    t.env = c.env;
    t.matchingVal = c.matchingVal;

    c.isMatch = h.isMatch || t.isMatch;
    c.value = if h.isMatch
                then h.value
                else t.value;
}

-- Concrete syntax

nonterminal MatchCase_c with ast_Match;
synthesized attribute ast_Match :: MatchCase;

concrete production matchCase_c
c::MatchCase_c ::= Bar p::Pat_c Arrow e::Expr_c
{
    c.ast_Match = matchCase(p.ast_Pat, e.ast);
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
e::Expr_c ::= Match target::Expr_c With cases::MatchCases_c End
{
    e.ast = matchExpr(target.ast, cases.ast_Matches);
}