module Main (main) where

import Text.Parsec
import Text.Printf (printf)
import Text.Parsec.String (Parser)
import System.Environment (getArgs, getProgName)
import GHC.IO.Handle (hFlush)
import System.IO (stdout)
import Data.List.Split
import Control.Monad (foldM)

whitespace :: Parser ()
whitespace = skipMany (Text.Parsec.oneOf " \t\r\n")

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
        first <- Text.Parsec.oneOf ['1'..'9']
        rest <- many digit
        return (first : rest)

fractionPart :: Parser String
fractionPart = do
    dot <- char '.'
    digits <- many1 digit
    return (dot : digits)

exponentPart :: Parser String
exponentPart = do
    e <- Text.Parsec.oneOf "eE"
    sign <- option "" ((:[]) <$> Text.Parsec.oneOf "+-")
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
    values <- jsonValue `Text.Parsec.sepBy` symbol ","
    _ <- symbol "]"
    return $ JsonArray values

jsonObject :: Parser JsonValue
jsonObject = do
    _ <- symbol "{"
    m <- member `Text.Parsec.sepBy` symbol ","
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

getField :: JsonValue -> String ->Maybe JsonValue
getField (JsonObject fields) key =
    lookup key fields
getField _ _ =
    Nothing

getNestedField :: JsonValue -> String -> Maybe JsonValue
getNestedField (JsonObject fields) key = foldM getField (JsonObject fields) (splitOn "." key)
getNestedField _ _ = Nothing

repl :: FilePath -> IO ()
repl path = do
    parsedJson <- parseFile path
    putStr "> "
    hFlush stdout
    input <- getLine
    case parsedJson of
        Right value -> do
            print $ getNestedField value input
            repl path
        Left err -> print err


main :: IO ()
main = do
    progName <- getProgName
    args <- getArgs
    case args of
        [path] -> repl path
        _ -> printf "ERROR: No file path was provided.\nUSAGE:\n    %s <path>\n" progName
