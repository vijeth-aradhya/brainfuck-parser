import Text.Parsec
import Text.Parsec.String

data BFInstruction = GoBack | GoForward | Increment
                   | Decrement | Input | Output
                   | Loop [BFInstruction]
                   deriving (Show)

parseBack :: Parser BFInstruction
parseBack = parseGen '<' GoBack

parseForward :: Parser BFInstruction
parseForward = parseGen '>' GoForward

parseIncrement :: Parser BFInstruction
parseIncrement = parseGen '+' Increment

parseLoop :: Parser BFInstruction
parseLoop = do
    char '['
    insn <- parseInstructions
    char ']'
    return $ Loop insn

parseDecrement :: Parser BFInstruction
parseDecrement = parseGen '-' Decrement

parseInput :: Parser BFInstruction
parseInput = parseGen ',' Input

parseOutput :: Parser BFInstruction
parseOutput = parseGen '.' Output

parseGen :: Char -> BFInstruction -> Parser BFInstruction
parseGen x y = char x >> return y

parseComment :: Parser()
parseComment = do
    many $ noneOf "<>+-,.[]"
    return ()

parseInstruction :: Parser BFInstruction
parseInstruction = do
    parseComment
    i <- parseBack <|> parseForward <|> parseIncrement <|> parseDecrement 
         <|> parseInput <|> parseOutput <|> parseLoop
    parseComment
    return i

parseInstructions :: Parser [BFInstruction]
parseInstructions = many parseInstruction

main :: IO()
main = do
    cont <- readFile "tests/hello.bf"
    case parse parseInstructions "hello.bf" cont of
        Left e -> print e
        Right insn -> print insn

{-

The process of compiling or interpreting programs requires, as one of its steps, parsing the source code and structuring them. More specifically, we have a step to convert a string into a set of tokens, called lexical analysis, which is carried out by a lexer or tokenizer.

After that, we structure those tokens in a such a way that encodes meaning of these tokens, such as abstract syntax trees (AST). This step is called parsing and is out by a parser.
	
-}
