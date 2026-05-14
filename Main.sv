grammar evaluator;

global quitCommands :: [String] = ["q", "quit", "Q", "QUIT"];
global loadCommand :: String = "eval";

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
    print("> ");
    userInput <- readLineStdin();
    let command =
      case userInput of
      | nothing() -> ""
      | just(ln) -> ln
      end;

    if containsBy(stringEq, command, quitCommands)
    then do {
      return 0;
    }
    else do {
      let isLoadCmd = substring(0, 4, command) == loadCommand;
      
      expr <- 
        if isLoadCmd 
        then readFile("evaluator/examples/" ++ substring(5, length(command), command) ++ ".eval") 
        else do { return command; };
      
      let result = parse(expr, "User Input");

      if result.parseSuccess then do {
        let cst = result.parseTree;
        let ast = cst.astRoot;

        if !null(ast.typeErrors) then do {
          print("Error:\n" ++ implode("\n\t", ast.typeErrors) ++ "\n");
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

function stringEq
Boolean ::= l::String r::String
{
  return l == r;
}