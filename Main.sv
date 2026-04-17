grammar evaluator ;

parser parse :: Root_c
{
  evaluator;
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
        let r_ast = r_cst.ast_Root;
        
        print("Value: " ++ toStringValue(r_ast.value) ++ "\n\n");
        return 0;
      }
      else do {
        print("Encountered a parse error:\n" ++ result.parseErrors ++ "\n");
        return 1;
      };
    };
  };
}