{-# OPTIONS -Wall #-}

module Environment where

import Control.Monad.Except
import Data.IORef
import Data.Maybe

import Types

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var) (liftIO . readIORef) (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var) (liftIO . flip writeIORef value) (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  isAlreadyDefined <- liftIO $ isBound envRef var
  if isAlreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
           valueRef <- newIORef value
           env <- readIORef envRef
           writeIORef envRef ((var, valueRef) : env)
           return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bs env = fmap (++ env) (mapM addBinding bs)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)