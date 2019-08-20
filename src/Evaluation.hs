{-# OPTIONS -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}

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
eval (List [Atom "if", p, conseq, alt]) = do
  res <- eval p
  case res of
    Bool False -> eval alt
    Bool True -> eval conseq
    _ -> throwError $ TypeMismatch "bool" p
eval form@(List (Atom "cond":clauses)) = cond form clauses
eval form@(List (Atom "case":key:clauses)) = cases form key clauses
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
  , ("=", numBoolBinop (==))
  , ("<", numBoolBinop (<))
  , (">", numBoolBinop (>))
  , ("/=", numBoolBinop (/=))
  , (">=", numBoolBinop (>=))
  , ("<=", numBoolBinop (<=))
  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))
  , ("string=?", strBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  , ("symbol?", unaryOp symbolP)
  , ("string?", unaryOp stringP)
  , ("number?", unaryOp numberP)
  , ("bool?", unaryOp boolP)
  , ("list?", unaryOp listP)
  , ("string->symbol", unaryOpThrows string2symbol)
  , ("symbol->string", unaryOpThrows symbol2string)
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("eq?", eqv)
  , ("eqv?", eqv)
  , ("equal?", equal)
  ]

cond :: LispVal -> [LispVal] -> Either LispError LispVal
cond form clauses =
  if null clauses
    then throwError $ BadSpecialForm "no true clauses in cond expression: " form
    else case head clauses of
           List [Atom "else", expr] -> eval expr
           List [t, expr] -> eval $ List [Atom "if", t, expr, List (Atom "cond" : tail clauses)]
           _ -> throwError $ BadSpecialForm "ill-formed cond expression: " form

cases :: LispVal -> LispVal -> [LispVal] -> Either LispError LispVal
cases form key clauses =
  if null clauses
    then throwError $ BadSpecialForm "no true clause in case expression: " form
    else case head clauses of
           List (Atom "else":exprs) -> last <$> mapM eval exprs
           List (List xs:exprs) -> do
             result <- eval key
             equality <- mapM (\x -> eqv [result, x]) xs
             if Bool True `elem` equality
               then last <$> mapM eval exprs
               else eval $ List (Atom "case" : key : tail clauses)
           _ -> throwError $ BadSpecialForm "ill-formed case expression: " form

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [bad] = throwError $ TypeMismatch "pair" bad
car bad = throwError $ NumArgs 1 bad

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [bad] = throwError $ TypeMismatch "pair" bad
cdr bad = throwError $ NumArgs 1 bad

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool x, Bool y] = return $ Bool $ x == y
eqv [Number x, Number y] = return $ Bool $ x == y
eqv [String x, String y] = return $ Bool $ x == y
eqv [Atom x, Atom y] = return $ Bool $ x == y
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [l1@(List _), l2@(List _)] = eqvList eqv [l1, l2]
eqv [_, _] = return $ Bool False
eqv bad = throwError $ NumArgs 2 bad

data Unpacker =
  forall a. Eq a =>
            AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
     `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [l1@(List _), l2@(List _)] = eqvList equal [l1, l2]
equal [DottedList xs x, DottedList ys y] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] = do
  primitiveEquals <-
    or <$>
    mapM (unpackEquals arg1 arg2) [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $
    Bool
      (primitiveEquals ||
       let (Bool x) = eqvEquals
        in x)
equal badArgList = throwError $ NumArgs 2 badArgList

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