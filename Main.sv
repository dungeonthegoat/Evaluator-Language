grammar evaluator;

parser parse :: Root_c
{
  evaluator;
}

function main 
IO<Integer> ::= args::[String]
{
  return do {
    result <- processUserInput();
    return result;
  };
}

function processUserInput
IO<Integer> ::=
{
  return do {
    userInput <- readLineStdin();
    let command =
        case userInput of
        | nothing() -> ""
        | just(ln) -> ln
        end;

    if command == "q" || command == "quit" || command == "QUIT" || command == "Q"
    then do {
      return 0;
    }
    else do {
      content <- readFile("evaluator/examples/" ++ command ++ ".eval");
      let result = parse(content, "User Input");

      if result.parseSuccess then do {
        let cst = result.parseTree;
        let ast = cst.astRoot;

        if !null(ast.typeErrors) then do {
          print(implode("\n\t", ast.typeErrors) ++ "\n");
        }
        else do {
          print("=: " ++ toStringValue(ast.value) ++ "\n");
        };
      }
      else do {
        print("Error parsing:\n" ++ result.parseErrors ++ "\n");
      };

      processUserInput();
    };
  };
}