{-# OPTIONS -Wall #-}

module Repl
  ( runRepl
  , evalAndPrint
  , runOne
  ) where

import Control.Monad.Except
import Data.String.Utils

import System.IO
import Text.ParserCombinators.Parsec

import Environment
import Errors
import Evaluation
import Parsing
import Types

readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right x -> return x

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = strip <$> (flushStr prompt >> getLine)

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ p prompt action = do
  res <- prompt
  if p res
    then return ()
    else action res >> until_ p prompt action

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "List>>> ") . evalAndPrint