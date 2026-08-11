module Main (main) where

import Text.Parsec
import Text.Printf (printf)
import Text.Parsec.String (Parser)
import System.Environment (getArgs, getProgName)
import GHC.IO.Handle (hFlush)
import System.IO (stdout)
import Data.Either.Combinators (mapLeft)
import Data.Either.Utils
import Control.Monad (foldM)

whitespace :: Parser ()
whitespace = skipMany (Text.Parsec.oneOf " \t\r\n")

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

symbol :: String -> Parser String
symbol = lexeme . string

data JsonPath =
    PathRoot String |
    PathAcess String |
    PathIndex Int
    deriving(Show, Eq)

jsonPath :: Parser [JsonPath]
jsonPath = many jsonPathComponent <* eof

jsonPathComponent :: Parser JsonPath
jsonPathComponent =
    jsonPathAcess <|>
    jsonPathIndex <|>
    jsonPathRoot

jsonPathRoot :: Parser JsonPath
jsonPathRoot = PathRoot <$> many1 (noneOf ".[]")

jsonPathAcess :: Parser JsonPath
jsonPathAcess = PathAcess <$> (symbol "." *> many1 (noneOf ".[]"))

jsonPathIndex :: Parser JsonPath
jsonPathIndex = PathIndex <$> read <$> (char '[' *> many1 digit <* char ']')

data QueryError =
    MalformedPathError ParseError|
    InexsitentPathError

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

serialize :: Int -> JsonValue -> String
serialize ident value = case value of
    JsonNull -> "null"
    JsonBool b -> if b then "true" else "false"
    JsonString s -> "\"" ++ s ++ "\""
    JsonNumber d -> show d
    JsonArray a -> "[\n" ++ unlines [prefix ++ serialize (ident + 1) v ++ ","| v <- a] ++ prev_prefix ++ "]"
    JsonObject pairs -> "{\n" ++ unlines [prefix ++ show k ++ ":" ++ serialize (ident + 1) v ++ ","| (k, v) <- pairs] ++ prev_prefix ++ "}"
    where
        prefix = replicate (ident * 4) ' '
        prev_prefix = replicate ((ident - 1) * 4) ' '

parseFile :: FilePath -> IO (Either ParseError JsonValue)
parseFile path = do
    content <- readFile path
    pure $ parse json path content

getField :: JsonValue -> String ->Maybe JsonValue
getField (JsonObject fields) key =
    lookup key fields
getField _ _ =
    Nothing

getArrayMember :: JsonValue -> Int -> Maybe JsonValue
getArrayMember (JsonArray array) index =
    pure $ array !! index
getArrayMember _ _ =
    Nothing

resolvePathEntry :: JsonValue -> JsonPath -> Maybe JsonValue
resolvePathEntry obj (PathRoot p) = getField obj p
resolvePathEntry obj (PathAcess p) = getField obj p
resolvePathEntry obj (PathIndex i) = getArrayMember obj i

getNestedField :: JsonValue -> String -> Either QueryError JsonValue
getNestedField object path = do
    parsedPath <- mapLeft (\e -> MalformedPathError e) $ parse jsonPath "" path
    maybeToEither InexsitentPathError (foldM resolvePathEntry object parsedPath)


repl :: Either ParseError JsonValue -> IO ()
repl parsedJson = do
    putStr "> "
    hFlush stdout
    input <- getLine
    case parsedJson of
        Right value -> do
            case getNestedField value input of
                Right j -> putStrLn $ serialize 1 j
                Left InexsitentPathError -> putStrLn $ "field '" ++ input ++ "' does not exist"
                Left (MalformedPathError e) -> putStrLn $ "the path '" ++ input ++ "' is not a valid one: " ++ show e
            repl parsedJson
        Left err -> print err


main :: IO ()
main = do
    progName <- getProgName
    args <- getArgs
    case args of
        [path] -> do
            parsedJson <- parseFile path
            case parsedJson of
                Right rParsedJson -> do
                    putStrLn $ serialize 1 rParsedJson
                    repl parsedJson
                Left err -> print err
        _ -> printf "ERROR: No file path was provided.\nUSAGE:\n    %s <path>\n" progName
