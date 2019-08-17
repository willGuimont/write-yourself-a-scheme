{-# OPTIONS -Wall #-}

module Main where

import Parsing
import System.Environment
import Text.ParserCombinators.Parsec

readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr