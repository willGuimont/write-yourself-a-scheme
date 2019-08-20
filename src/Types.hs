{-# OPTIONS -Wall #-}

module Types where

import Data.Array (Array)
import Data.Complex
import Data.Foldable

data LispVal
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
  deriving (Eq)

instance Show LispVal where
  show = showVal

showVal :: LispVal -> String
showVal (Atom x) = x
showVal (List xs) = "(" ++ unwords (map showVal xs) ++ ")"
showVal (DottedList xs y) = "(" ++ unwords (map showVal xs) ++ " . " ++ showVal y ++ ")"
showVal (Number x) = show x
showVal (String x) = x
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character c) = [c]
showVal (Float x) = show x
showVal (Ratio x) = show x
showVal (Complex x) = show x
showVal (Vector xs) = "#(" ++ unwords (map showVal (toList xs)) ++ ")"