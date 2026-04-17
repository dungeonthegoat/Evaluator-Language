grammar evaluator;

ignore terminal WhiteSpace /[\t\r\n\ ]+/;
ignore terminal Comment    /\-\-[^\n\r]*/;

lexer class KEYWORDS;

-- Arithmetic

terminal Star   '*'     precedence = 12, association = left;
terminal Slash  '/'     precedence = 12, association = left;
terminal Modulo '%'     precedence = 12, association = left;
terminal Plus   '+'     precedence = 10, association = left;
terminal Dash   '-'     precedence = 10, association = left;

-- Lists

terminal ConsOp '::'        precedence = 9, association = right;
terminal Append '@'         precedence = 9, association = right;
terminal LeftBracket '['    precedence = 8, association = left;
terminal RightBracket ']'   precedence = 8, association = left;
terminal Bar '|'            precedence = 8, association = left;

-- Comparisons

terminal Less    '<'        precedence = 7, association = left;
terminal Greater '>'        precedence = 7, association = left;
terminal LessEq     '<='    precedence = 7, association = left;
terminal GreaterEq  '>='    precedence = 7, association = left;
terminal EqOp   '=='        precedence = 6,  association = left;

-- Boolean Logic

terminal Not    '!'     precedence = 8, association = left, lexer classes {KEYWORDS};
terminal And    '&&'    precedence = 5, association = left, lexer classes {KEYWORDS};
terminal Or     '||'    precedence = 4, association = left, lexer classes {KEYWORDS};

-- Keywords

terminal Eq     '='             precedence = 2, association = left;
terminal In     'in'            precedence = 2, association = left,     lexer classes {KEYWORDS};
terminal Let    'let'           precedence = 2, association = left,     lexer classes {KEYWORDS};
terminal Rec    'rec'           precedence = 2, association = left,     lexer classes {KEYWORDS};
terminal If     'if'            precedence = 2, association = left,     lexer classes {KEYWORDS};
terminal Then   'then'          precedence = 2, association = left,     lexer classes {KEYWORDS};
terminal Else   'else'          precedence = 2, association = left,     lexer classes {KEYWORDS};
terminal LambdaParam 'lambda'   precedence = 2, association = right,    lexer classes {KEYWORDS};
terminal Arrow     '->'         precedence = 2, association = right;
terminal Match 'match'          precedence = 2, association = left,     lexer classes {KEYWORDS};
terminal With 'with'            precedence = 2, association = left,     lexer classes {KEYWORDS};
terminal End 'end'              precedence = 2, association = left,     lexer classes {KEYWORDS};
terminal Sep ','                precedence = 2, association = left;

-- Value Literals

terminal True   'true'  lexer classes {KEYWORDS};
terminal False  'false' lexer classes {KEYWORDS};
terminal Var  /[a-zA-Z_][a-zA-Z0-9_]*/ submits to {KEYWORDS};
terminal IntLit /-?[0-9]+/;

-- Other

terminal LeftParen  '(' precedence = 15, association = left;
terminal RightParen ')' precedence = 15, association = left;