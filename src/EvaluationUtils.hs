{-# OPTIONS -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}

module EvaluationUtils where

import Control.Monad.Except
import Data.Char (toLower)

import Types

isTrue :: LispVal -> Bool
isTrue (Bool x) = x
isTrue _ = False

hasTrueIn :: [LispVal] -> Bool
hasTrueIn = any isTrue

data Unpacker =
  forall a. Eq a =>
            AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
     `catchError` const (return False)

eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [List arg1, List arg2] = return $ Bool $ (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
  where
    eqvPair (x1, x2) =
      case eqvFunc [x1, x2] of
        Left _ -> False
        Right (Bool val) -> val
        _ -> False
eqvList _ xs = throwError $ TypeMismatch "list" (List xs)

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

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op xs =
  if length xs /= 2
    then throwError $ NumArgs 2 xs
    else do
      left <- unpacker $ head xs
      right <- unpacker $ xs !! 1
      return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr other = throwError $ TypeMismatch "string" other

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool other = throwError $ TypeMismatch "boolean" other

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return $ f v
unaryOp _ xs = throwError $ NumArgs 1 xs

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

unaryOpThrows :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOpThrows f [v] = f v
unaryOpThrows _ xs = throwError $ NumArgs 1 xs

symbol2string :: LispVal -> ThrowsError LispVal
symbol2string (Atom s) = return $ String $ map toLower s
symbol2string x = throwError $ TypeMismatch "atom" x

string2symbol :: LispVal -> ThrowsError LispVal
string2symbol (String s) = return $ Atom s
string2symbol x = throwError $ TypeMismatch "string" x