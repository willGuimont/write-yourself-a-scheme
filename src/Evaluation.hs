{-# OPTIONS -Wall #-}

module Evaluation
  ( eval
  ) where

import Control.Monad.Except
import Data.Char (toLower)

import Errors
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
-- TODO missing evals
eval :: LispVal -> ThrowsError LispVal
eval x@(String _) = return x
eval x@(Number _) = return x
eval x@(Bool _) = return x
eval (List [Atom "quote", x]) = return x
eval (List (Atom f:xs)) = mapM eval xs >>= apply f
eval bad = throwError $ BadSpecialForm "Unrecognized special form" bad

apply :: String -> [LispVal] -> ThrowsError LispVal
apply f xs = maybe (throwError $ NotFunction "Unrecognized primitive function args" f) ($ xs) (lookup f primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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

-- TODO not only integer
numericBinaryOperation :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinaryOperation _ [] = throwError $ NumArgs 2 []
numericBinaryOperation _ x@[_] = throwError $ NumArgs 2 x
numericBinaryOperation f xs = Number . foldl1 f <$> mapM unpackNum xs

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum bad = throwError $ TypeMismatch "number" bad

unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = f v
unaryOp _ xs = throwError $ NumArgs 1 xs

symbolP, stringP, numberP, boolP, listP :: LispVal -> ThrowsError LispVal
symbolP (Atom _) = return $ Bool True
symbolP _ = return $ Bool False

stringP (String _) = return $ Bool True
stringP _ = return $ Bool False

numberP (Number _) = return $ Bool True
numberP _ = return $ Bool False

boolP (Bool _) = return $ Bool True
boolP _ = return $ Bool False

listP (List _) = return $ Bool True
listP (DottedList _ _) = return $ Bool True
listP _ = return $ Bool False

symbol2string :: LispVal -> ThrowsError LispVal
symbol2string (Atom s) = return $ String $ map toLower s
symbol2string x = throwError $ TypeMismatch "atom" x

string2symbol :: LispVal -> ThrowsError LispVal
string2symbol (String s) = return $ Atom s
string2symbol x = throwError $ TypeMismatch "string" x
