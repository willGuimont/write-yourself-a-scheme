{-# OPTIONS -Wall #-}

module Errors where

import Control.Monad.Except

import Types

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right x) = x
extractValue _ = undefined

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right x) = return x

runIOThrows :: IOThrowsError String -> IO String
runIOThrows f = extractValue <$> runExceptT (trapError f)