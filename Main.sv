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
          print("E: " ++ toStringValue(ast.value) ++ "\n");
        };
      }
      else do {
        print("Error parsing:\n" ++ result.parseErrors ++ "\n");
      };

      processUserInput();
    };
  };
}

-- if null(args) then do {
--       print("No arguments provided\n");
--       return 1;
--     }

--     else do {
--       let fileName = head(args);
--       content <- readFile(fileName);
--       let result = parse(content, fileName);

--       if result.parseSuccess then do {
--         let r_cst = result.parseTree;
--         let r_ast = r_cst.astRoot;
        
--         if !null(r_ast.typeErrors) then do {
--           print("Error Evaluating!\nType Errors:\n\t" ++ implode("\n\t", r_ast.typeErrors) ++ "\n");
--           return 1;
--         } 
--         else do {
--           print("Value: " ++ toStringValue(r_ast.value) ++ "\n");
--           print("Type:  " ++ toStringType(r_ast.type) ++ "\n");
--           -- print("PP: " ++ r_ast.pp ++ "\n\n");
--           return 0;
--         };
--       }
--       else do {
--         print("Encountered a parse error:\n" ++ result.parseErrors ++ "\n");
--         return 1;
--       };
--     };