grammar evaluator;

nonterminal Pattern with matchingVal, isMatch, matchedVars;

inherited attribute matchingVal :: Value;
synthesized attribute isMatch :: Boolean;
synthesized attribute matchedVars :: [Pair<String Value>];

-- Productions

abstract production emptyListPat
p::Pattern ::=
{
    p.isMatch = case p.matchingVal of
        | emptyListVal() -> true
        | _ -> false
    end;
    p.matchedVars = [];
}

abstract production consPat
p::Pattern ::= headName::String tailName::String
{
    p.isMatch = case p.matchingVal of
        | consVal(_, _) -> true
        | _ -> false
    end;
    p.matchedVars = case p.matchingVal of
        | consVal(h, t) -> [pair(fst = headName, snd = new(h)), pair(fst = tailName, snd = new(t))]
        | _ -> []
    end;
}

abstract production varPat
p::Pattern ::= name::String
{
    p.isMatch = true;
    p.matchedVars = [pair(fst = name, snd = p.matchingVal)];
}

-- Concrete syntax

concrete production emptyPat_c
p::Pat_c ::= '[' ']'
{
    p.ast_Pat = emptyListPat();
}

concrete production consPat_c
p::Pat_c ::= h::Var ConsOp t::Var
{
    p.ast_Pat = consPat(h.lexeme, t.lexeme);
}

concrete production varPat_c
p::Pat_c ::= v::Var
{
    p.ast_Pat = varPat(v.lexeme);
}