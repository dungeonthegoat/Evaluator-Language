grammar evaluator;

ignore terminal WhiteSpace /[\t\r\n\ ]+/;
ignore terminal Comment    /\-\-[^\n\r]*/;

lexer class KEYWORDS;

-- Arithmetic

terminal Pow   '^'      precedence = 14, association = right;
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
terminal Sep ','            precedence = 2, association = left;
terminal Dots '...'         precedence = 2, association = left;

-- Pattern Matching

terminal Case '?'           precedence = 2, association = left;
terminal Arrow     '->'     precedence = 2, association = right;
terminal Bar '|'            precedence = 8, association = left;
terminal LeftBrace '{'      precedence = 15, association = left;
terminal RightBrace '}'     precedence = 15, association = left;

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

-- Lambda & Binding

terminal Lambda 'lambda'    precedence = 2, association = right, lexer classes {KEYWORDS};
terminal Colon  ':'         precedence = 2, association = left;
terminal Eq     '='         precedence = 2, association = left;
terminal In     ';'         precedence = 2, association = left;

-- Keywords

terminal If     'if'          precedence = 2, association = left,     lexer classes {KEYWORDS};
terminal Then   'then'        precedence = 2, association = left,     lexer classes {KEYWORDS};
terminal Else   'else'        precedence = 2, association = left,     lexer classes {KEYWORDS};
terminal IntT   'int'         precedence = 2, association = left,     lexer classes {KEYWORDS};
terminal FloatT 'float'       precedence = 2, association = left,     lexer classes {KEYWORDS};
terminal BoolT  'bool'        precedence = 2, association = left,     lexer classes {KEYWORDS};
terminal AnyT   'any'         precedence = 2, association = left,     lexer classes {KEYWORDS};
terminal ADT    'type'        precedence = 2, association = left,     lexer classes {KEYWORDS};
terminal Let    'let'         precedence = 2, association = left,     lexer classes {KEYWORDS};

-- Value Literals

terminal True   'true'  lexer classes {KEYWORDS};
terminal False  'false' lexer classes {KEYWORDS};
terminal Var  /[a-z_][a-zA-Z0-9_]*/ submits to {KEYWORDS};
terminal TypeName  /[A-Z][a-zA-Z0-9_]*/ submits to {KEYWORDS};
terminal IntLitT /(0|[1-9][0-9]*)/;
terminal FloatLitT /[0-9]+\.[0-9]+/;

-- Other

terminal LeftParen  '(' precedence = 15, association = left;
terminal RightParen ')' precedence = 15, association = left;