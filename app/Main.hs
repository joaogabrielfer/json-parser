module Main (main) where

import Text.Parsec
import Text.Printf (printf)
import Text.Parsec.String (Parser)
import System.Environment (getArgs, getProgName)

whitespace :: Parser ()
whitespace = skipMany (oneOf " \t\r\n")

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

symbol :: String -> Parser String
symbol = lexeme . string

data JsonValue =
    JsonNull |
    JsonBool Bool |
    JsonString String |
    JsonNumber Double |
    JsonArray [JsonValue] |
    JsonObject [(String, JsonValue)]
    deriving(Show, Eq)

jsonValue :: Parser JsonValue
jsonValue =
    jsonNull <|>
    jsonBool <|>
    jsonString <|>
    jsonNumber <|>
    jsonArray <|>
    jsonObject

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ symbol "null"

jsonBool :: Parser JsonValue
jsonBool =
        JsonBool True <$ symbol "true"
    <|> JsonBool False <$ symbol  "false"

jsonStringRaw :: Parser String
jsonStringRaw =
    (char '"' *> many (noneOf "\"") <* char '"')


jsonString :: Parser JsonValue
jsonString = JsonString <$> lexeme jsonStringRaw

integerPart :: Parser String
integerPart =
        string "0"
    <|> do
        first <- oneOf ['1'..'9']
        rest <- many digit
        return (first : rest)

fractionPart :: Parser String
fractionPart = do
    dot <- char '.'
    digits <- many1 digit
    return (dot : digits)

exponentPart :: Parser String
exponentPart = do
    e <- oneOf "eE"
    sign <- option "" ((:[]) <$> oneOf "+-")
    digits <- many1 digit
    return (e : sign ++ digits)

jsonNumber :: Parser JsonValue
jsonNumber = do
    sign <- option "" (string "-")
    number <- integerPart
    fraction <- option "" fractionPart
    expo <- option "" exponentPart
    return $ JsonNumber $ read (sign ++ number ++ fraction ++ expo)

jsonArray :: Parser JsonValue
jsonArray = do
    _ <- symbol "["
    values <- jsonValue `sepBy` symbol ","
    _ <- symbol "]"
    return $ JsonArray values

jsonObject :: Parser JsonValue
jsonObject = do
    _ <- symbol "{"
    m <- member `sepBy` symbol ","
    _ <- symbol "}"
    return $ JsonObject m

member :: Parser (String, JsonValue)
member = do
    key <- lexeme jsonStringRaw
    _ <- symbol ":"
    value <- jsonValue
    return (key, value)

json :: Parser JsonValue
json = whitespace *> jsonValue <* eof

-- parseInput :: String -> Either ParseError JsonValue
-- parseInput s = parse json "<input>" s

parseFile :: FilePath -> IO (Either ParseError JsonValue)
parseFile path = do
    content <- readFile path
    pure $ parse json path content

main :: IO ()
main = do
    progName <- getProgName
    args <- getArgs
    case args of
        [path] -> do
            parsedJson <- parseFile path
            print $ parsedJson
        _ -> printf "ERROR: No file path was provided.\nUSAGE:\n    %s <path>\n" progName
