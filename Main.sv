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
    return case args of
    | fileName::_ -> do {
            fileExists <- isFile(fileName);
            if !fileExists then do {
                print("File not found\n");
                return 1;
            }
            else do {
                expr <- readFile(fileName);
                evalString(expr, fileName);
            };
        }
    | [] -> processUserInput()
    end;
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
            let isLoadCmd = length(command) > 5 && substring(0, 4, command) == loadCommand;
            let fileName = "evaluator/examples/" ++ substring(5, length(command), command) ++ ".eval";

            expr <- 
                if isLoadCmd 
                then readFile(fileName) 
                else do { return command; };
            
            let sourceName = if isLoadCmd then fileName else "User Input";
            let evaluated = evalString(expr, sourceName);

            processUserInput();
        };
    };
}

function evalString
IO<Integer> ::= expr::String sourceName::String
{
    return do {
        let result = parse(expr, sourceName);

        if result.parseSuccess then do {
            let cst = result.parseTree;
            let ast = cst.astRoot;

            if !null(ast.typeErrors) then do {
                print("Error:\n" ++ implode("\n\t", ast.typeErrors) ++ "\n");
                return 1;
            }
            else do {
                print("=: " ++ toStringValue(ast.value) ++ "\n");
                print("type: " ++ toStringType(ast.type) ++ "\n");
                return 0;
            };
        }
        else do {
            print("Error parsing:\n" ++ result.parseErrors ++ "\n");
            return 1;
        };
    };
}

function stringEq
Boolean ::= l::String r::String
{
  return l == r;
}