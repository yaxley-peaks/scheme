{-# OPTIONS_GHC -Wno-deprecations #-}

module Vars where

import Control.Monad.Error (ErrorT (runErrorT), MonadError (throwError), MonadIO (liftIO))
import Data (LispVal)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Maybe (isJust)
import Error (LispError (..), ThrowsError, extractValue, trapError)

type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ErrorT LispError IO

liftThrows :: MonadError e m => Either e a -> m a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runErrorT (trapError action)

isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)



nullEnv = newIORef []
