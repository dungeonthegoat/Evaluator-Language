nonterminal Library with libEnv, libTypeEnv;
nonterminal Decl;


-- Environments to be propagated UP from declarations
monoid attribute libEnv :: [(String, Value)] with [], ++;
monoid attribute libTypeEnv :: [(String, Value)] with [], ++;