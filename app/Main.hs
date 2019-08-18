{-# OPTIONS -Wall #-}

module Main where

import Control.Monad.Except
import Parsing
import System.Environment
import Text.ParserCombinators.Parsec

import Evaluation
import Types
import Errors

readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right x -> return x

main :: IO ()
main = do
  args <- getArgs
  let evaluated = fmap show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaluated