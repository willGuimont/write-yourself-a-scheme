{-# OPTIONS -Wall #-}

module Types where

import Control.Monad.Except
import Data.Array (Array)
import Data.Complex
import Data.Foldable
import Data.IORef
import Text.ParserCombinators.Parsec
import System.IO (Handle)

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
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func
      { params :: [String]
      , vararg :: Maybe String
      , body :: [LispVal]
      , closure :: Env
      }
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Port Handle

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
showVal (PrimitiveFunc _) = "<primitive>"
showVal Func {params = args, vararg = varargs} =
  "(lambda (" ++
  unwords (map show args) ++
  (case varargs of
     Nothing -> ""
     Just arg -> " . " ++ arg) ++
  ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

instance Show LispError where
  show = showError

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args: found values " ++ unwords (map show found)
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default s) = s

type IOThrowsError = ExceptT LispError IO

type ThrowsError = Either LispError

type Env = IORef [(String, IORef LispVal)]