grammar evaluator ;

parser parse :: Root_c
{
  evaluator;
  evaluator:derivative;
}

function main 
IO<Integer> ::= args::[String]
{
  return do {
    if null(args) then do {
      print("No arguments provided\n");
      return 1;
    }

    else do {
      let fileName = head(args);
      content <- readFile(fileName);
      let result = parse(content, fileName);

      if result.parseSuccess then do {
        let r_cst = result.parseTree;
        let r_ast = r_cst.astRoot;
        
        if !null(r_ast.typeErrors) then do {
          print("Error Evaluating!\nType Errors:\n\t" ++ implode("\n\t", r_ast.typeErrors) ++ "\n");
          return 1;
        } 
        else do {
          print("Value: " ++ toStringValue(r_ast.value) ++ "\n\n");
          print("Type: " ++ toStringType(r_ast.type) ++ "\n\n");
          print("PP: " ++ r_ast.pp ++ "\n\n");
          return 0;
        };
      }
      else do {
        print("Encountered a parse error:\n" ++ result.parseErrors ++ "\n");
        return 1;
      };
    };
  };
}