grammar evaluator ;

parser parse :: Root_c
{
  evaluator;
}

function main 
IO<Integer> ::= args::[String]
{
  return do {
    -- Argument does not exist
    if null(args) then do {
      print("No arguments provided\n");
      return 1;
    }

    -- Argument does exist
    else do {
      -- File reading and parsing
      let fileName = head(args);
      content <- readFile(fileName);
      let result = parse(content, fileName);

      -- Parse is successful
      if result.parseSuccess then do {
        let r_cst = result.parseTree;
        let r_ast = r_cst.ast_Root;
        
        print("Value: " ++ toStringValue(r_ast.value) ++ "\n\n");
        return 0;
      } 

      -- Parse fails
      else do {
        print("Encountered a parse error:\n" ++ result.parseErrors ++ "\n");
        return 1;
      };
    };
  };
}