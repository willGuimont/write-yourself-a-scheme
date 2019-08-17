module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

import Control.Monad (liftM)
import Data.Char (toLower, toUpper)
import Data.Complex
import Data.Ratio
import Lib
import Numeric
import Data.Array (Array)
import Data.Array.Base (listArray)

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Character Char
  | Float Double
  | Ratio Rational
  | Complex (Complex Double)
  | Vector (Array Int LispVal)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $
    case atom of
      "#t" -> Bool True
      "#f" -> Bool False
      _ -> Atom atom

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseAnyList :: Parser LispVal
parseAnyList = do
  char '('
  x <- try parseList <|> parseDottedList
  char ')'
  return x

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnquoted :: Parser LispVal
parseUnquoted = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseDecimal :: Parser LispVal
parseDecimal = parseDecimal1 <|> parseDecimal2

parseDecimal1 :: Parser LispVal
parseDecimal1 = Number . read <$> many1 digit

parseDecimal2 :: Parser LispVal
parseDecimal2 = do
  try $ string "#d"
  x <- many1 digit
  (return . Number . read) x

parseOctal :: Parser LispVal
parseOctal = do
  try $ string "#o"
  x <- many1 digit
  (return . Number . fst . (!! 0) . readOct) x

parseHexadecimal :: Parser LispVal
parseHexadecimal = do
  try $ string "#x"
  x <- many1 digit
  (return . Number . fst . (!! 0) . readHex) x

parseBinary :: Parser LispVal
parseBinary = do
  try $ string "#b"
  x <- many1 (oneOf "01")
  (return . Number . readBin) x

readBin :: String -> Integer
readBin = readBin' 0

readBin' :: Integer -> String -> Integer
readBin' c "" = c
readBin' c (x:xs) = readBin' cc xs
  where
    cc =
      2 * c +
      if x == '0'
        then 0
        else 1

parseNumber :: Parser LispVal
parseNumber = parseDecimal <|> parseHexadecimal <|> parseOctal <|> parseBinary

escape :: Parser String
escape = do
  d <- char '\\'
  c <- oneOf "\\\"0nrvtbf"
  return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

parseString :: Parser LispVal
parseString = do
  char '"'
  xs <- many character
  char '"'
  return $ String $ concat xs

parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

caseInsensitiveString :: String -> Parser String
caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""

parseCharacter :: Parser LispVal
parseCharacter = do
  try $ string "#\\"
  val <-
    try (caseInsensitiveString "newline" <|> caseInsensitiveString "space") <|> do
      x <- anyChar
      notFollowedBy alphaNum
      return [x]
  return $
    Character $
    case val of
      "newline" -> '\n'
      "space" -> ' '
      _ -> head val

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  char '.'
  y <- many1 digit
  (return . Float . fst . head . readFloat) (x ++ "." ++ y)

parseRatio :: Parser LispVal
parseRatio = do
  x <- many1 digit
  char '/'
  y <- many1 digit
  return $ Ratio (read x % read y)

toDouble :: LispVal -> Double
toDouble (Float f) = realToFrac f
toDouble (Number n) = fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do
  x <- try parseFloat <|> parseDecimal
  char '+'
  y <- try parseFloat <|> parseDecimal
  char 'i'
  return $ Complex (toDouble x :+ toDouble y)
  
parseVector :: Parser LispVal 
parseVector = do
  string "#("
  x <- parseVector'
  char ')'
  return x

parseVector' :: Parser LispVal
parseVector' = do
  values <- sepBy parseExpr spaces
  return $ Vector (listArray (0, length values - 1) values)

parseExpr :: Parser LispVal
parseExpr =
  parseAtom <|> parseString <|> try parseFloat <|> parseRatio <|> parseComplex <|> try parseNumber <|> parseQuoted <|>
  parseQuasiQuoted <|>
  parseUnquoted <|>
  parseVector <|>
  parseAnyList <|>
  try parseBool <|>
  try parseCharacter

readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr