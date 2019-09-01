{-# OPTIONS -Wall #-}

module Evaluation
  ( eval
  , primitiveBindings
  ) where

import Control.Monad.Except

import Data.Maybe (isNothing)
import Environment
import Errors
import EvaluationUtils
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
eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ x@(String _) = return x
eval _ x@(Number _) = return x
eval _ x@(Bool _) = return x
eval env (Atom name) = getVar env name
eval _ (List [Atom "quote", x]) = return x
eval env (List [Atom "if", p, conseq, alt]) = do
  res <- eval env p
  case res of
    Bool False -> eval env alt
    Bool True -> eval env conseq
    _ -> throwError $ TypeMismatch "bool" p
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env form@(List (Atom "cond":clauses)) = cond env form clauses
eval env form@(List (Atom "case":key:clauses)) = cases env form key clauses
eval env (List (Atom "define":List (Atom var:ps):b)) = makeNormalFunc env ps b >>= defineVar env var
eval env (List (Atom "define":DottedList (Atom var:ps) varargs:b)) = makeVarArgs varargs env ps b >>= defineVar env var
eval env (List (Atom "lambda":List ps:b)) = makeNormalFunc env ps b
eval env (List (Atom "lambda":DottedList ps var:b)) = makeVarArgs var env ps b
eval env (List (Atom "lambda":var@(Atom _):b)) = makeVarArgs var env [] b
eval env (List (f:xs)) = do
  ef <- eval env f
  val <- mapM (eval env) xs
  apply ef val
eval _ bad = throwError $ BadSpecialForm "Unrecognized special form" bad

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc f) xs = liftThrows $ f xs
apply (Func ps varargs b env) args =
  if num ps /= num args && isNothing varargs
    then throwError $ NumArgs (num ps) args
    else liftIO (bindVars env $ zip ps args) >>= bindVarArgs varargs >>= evalBody
  where
    remainingArgs = drop (length ps) args
    num = toInteger . length
    evalBody e = last <$> mapM (eval e) b
    bindVarArgs arg e =
      case arg of
        Just argName -> liftIO $ bindVars e [(argName, List remainingArgs)]
        Nothing -> return e

makeFunc :: Monad m => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env ps b = return $ Func (map showVal ps) varargs b env

makeNormalFunc :: Monad m => Env -> [LispVal] -> [LispVal] -> m LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: Monad m => LispVal -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeVarArgs = makeFunc . Just . showVal

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map makePrimitiveFunc primitives)
  where
    makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

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

cond :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
cond env form clauses =
  if null clauses
    then throwError $ BadSpecialForm "no true clauses in cond expression: " form
    else case head clauses of
           List [Atom "else", expr] -> eval env expr
           List [t, expr] -> eval env $ List [Atom "if", t, expr, List (Atom "cond" : tail clauses)]
           _ -> throwError $ BadSpecialForm "ill-formed cond expression: " form

cases :: Env -> LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal
cases env form key clauses =
  if null clauses
    then throwError $ BadSpecialForm "no true clause in case expression: " form
    else case head clauses of
           List (Atom "else":exprs) -> last <$> mapM (eval env) exprs
           List (List xs:exprs) -> do
             result <- eval env key
             equality <- liftThrows $ mapM (\x -> eqv [result, x]) xs
             if hasTrueIn equality
               then last <$> mapM (eval env) exprs
               else eval env $ List (Atom "case" : key : tail clauses)
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

equal :: [LispVal] -> ThrowsError LispVal
equal [l1@(List _), l2@(List _)] = eqvList equal [l1, l2]
equal [DottedList xs x, DottedList ys y] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] = do
  primitiveEquals <-
    or <$> mapM (unpackEquals arg1 arg2) [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $
    Bool
      (primitiveEquals ||
       let (Bool x) = eqvEquals
        in x)
equal badArgList = throwError $ NumArgs 2 badArgList