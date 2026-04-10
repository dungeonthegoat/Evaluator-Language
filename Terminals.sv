grammar evaluator;

ignore terminal WhiteSpace /[\t\r\n\ ]+/;
ignore terminal Comment    /\-\-[^\n\r]*/;

terminal Star   '*'     precedence = 12, association = left;
terminal Slash  '/'     precedence = 12, association = left;
terminal Modulo '%'     precedence = 11, association = left;
terminal Plus   '+'     precedence = 10, association = left;
terminal Dash   '-'     precedence = 10, association = left;
terminal EqOp   '=='    precedence = 8,  association = left;

terminal And    'and'   precedence = 6, association = left, lexer classes {KEYWORDS};
terminal Or     'or'    precedence = 6, association = left, lexer classes {KEYWORDS};
terminal Not    'not'   precedence = 6, association = left, lexer classes {KEYWORDS};

terminal LeftParen  '(' precedence = 15, association = left;
terminal RightParen ')' precedence = 15, association = left;

lexer class KEYWORDS;

terminal Eq      '='   precedence = 2, association = left;
terminal Less    '<'   precedence = 2, association = left;
terminal Greater '>'   precedence = 2, association = left;
terminal LessEq     '<='    precedence = 2, association = left;
terminal GreaterEq  '>='    precedence = 2, association = left;

terminal In     'in'   precedence = 2, association = left,   lexer classes {KEYWORDS};
terminal Let    'let'  precedence = 2, association = left,   lexer classes {KEYWORDS};
terminal Rec    'rec'  precedence = 2, association = left,   lexer classes {KEYWORDS};

terminal LambdaParam 'lambda' precedence = 1, association = right,   lexer classes {KEYWORDS};
terminal Arrow     '->' precedence = 1, association = right;

terminal True   'true'  lexer classes {KEYWORDS};
terminal False  'false' lexer classes {KEYWORDS};

terminal If     'if'    precedence = 2, association = left, lexer classes {KEYWORDS};
terminal Then   'then'  precedence = 2, association = left, lexer classes {KEYWORDS};
terminal Else   'else'  precedence = 2, association = left, lexer classes {KEYWORDS};

terminal Var  /[a-zA-Z][a-zA-Z0-9_]*/ submits to {KEYWORDS};
terminal IntLit /-?[0-9]+/;