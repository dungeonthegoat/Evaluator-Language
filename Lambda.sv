grammar evaluator;

abstract production letRecExpr
e::Expr ::= name::String param::String body::Expr e2::Expr
{
  e2.env = e.env ++ [pair(fst=name, snd=recClosureVal(name, param, new (body), e.env))];
  body.env = e.env;

  e.value = e2.value;
}

abstract production lambda
e::Expr ::= param::String b::Expr
{
  e.value = closureVal(param, new (b), e.env);
  b.env = e.env;
}

abstract production app
e::Expr ::= func::Expr arg::Expr
{
  arg.env = e.env;
  func.env = e.env;
  
  local attribute param :: String ;
  param = case func.value of
  | closureVal(p, _, _) -> p
  | recClosureVal(_, p, _, _) -> p
  | _ -> error("Type error: expected a function")
  end;

  local attribute body :: Expr ;
  body = case func.value of
  | closureVal(_, b, _) -> new (b)
  | recClosureVal(_, _, b, _) -> new (b)
  | _ -> error("Type error: expected a function")
  end;

  local attribute f_env :: [Pair<String Value>] ;
  f_env = case func.value of
  | closureVal(_, _, f) -> f
  | recClosureVal(name, _, _, f) -> f ++ [pair(fst=name, snd=func.value)]
  | _ -> []
  end;

  body.env = f_env ++ [pair(fst=param, snd=arg.value)] ;
  e.value = body.value;
}