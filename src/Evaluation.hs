{-# OPTIONS -Wall #-}

module Evaluation
  ( eval
  ) where

import Data.Char (toLower)
import Types

{-

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

-}
eval :: LispVal -> LispVal
eval x@(String _) = x
eval x@(Number _) = x
eval x@(Bool _) = x
eval (List [Atom "quote", x]) = x
eval (List (Atom f:xs)) = apply f $ map eval xs

apply :: String -> [LispVal] -> LispVal
apply f xs = maybe (Bool False) ($ xs) $ lookup f primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", numericBinaryOperation (+))
  , ("-", numericBinaryOperation (-))
  , ("*", numericBinaryOperation (*))
  , ("/", numericBinaryOperation div)
  , ("mod", numericBinaryOperation mod)
  , ("quotient", numericBinaryOperation quot)
  , ("remainder", numericBinaryOperation rem)
  , ("symbol?", unaryOp symbolP)
  , ("string?", unaryOp stringP)
  , ("number?", unaryOp numberP)
  , ("bool?", unaryOp boolP)
  , ("list?", unaryOp listP)
  , ("string->symbol", unaryOp string2symbol)
  , ("symbol->string", unaryOp symbol2string)
  ]

symbolP, stringP, numberP, boolP, listP :: LispVal -> LispVal
symbolP (Atom _) = Bool True
symbolP _ = Bool False

stringP (String _) = Bool True
stringP _ = Bool False

numberP (Number _) = Bool True
numberP _ = Bool False

boolP (Bool _) = Bool True
boolP _ = Bool False

listP (List _) = Bool True
listP (DottedList _ _) = Bool True
listP _ = Bool False

-- TODO not only integer
numericBinaryOperation :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinaryOperation f xs = Number $ foldl1 f $ map unpackNum xs

-- TODO else error
unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [xs] = f xs

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

-- TODO error
symbol2string :: LispVal -> LispVal
symbol2string (Atom s) = String $ map toLower s
symbol2string _ = String ""

-- TODO error
string2symbol :: LispVal -> LispVal
string2symbol (String s) = Atom s
string2symbol _ = Atom ""