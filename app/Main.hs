{-# OPTIONS -Wall #-}

module Main where

import Parsing
import System.Environment
import Text.ParserCombinators.Parsec

import Types
import Evaluation

readExpr :: String -> LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right x -> x

main :: IO ()
main = getArgs >>= print . eval . readExpr . head