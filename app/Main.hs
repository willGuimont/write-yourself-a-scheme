{-# OPTIONS -Wall #-}

module Main where

import Control.Monad.Except
import Data.String.Utils

import System.Environment
import System.IO
import Text.ParserCombinators.Parsec

import Types
import Errors
import Evaluation
import Parsing

readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right x -> return x

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = strip <$> (flushStr prompt >> getLine)

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (fmap show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ p prompt action = do
  res <- prompt
  if p res
    then return ()
    else action res >> until_ p prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "List>>> ") evalAndPrint

main :: IO ()
main = do
  args <- getArgs
  case Prelude.length args of
    0 -> runRepl
    1 -> evalAndPrint $ strip $ Prelude.head args
    _ -> putStrLn "Program takes only 0 or 1 argument"