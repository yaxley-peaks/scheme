{-# OPTIONS_GHC -Wno-deprecations #-}

module Vars where

import Control.Monad.Error (ErrorT (runErrorT), MonadError (throwError))
import Data (LispVal)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Maybe (isJust)
import Error (LispError, ThrowsError, extractValue, trapError)

type Env = [(String, IORef LispVal)]

type IOThrowsError = ErrorT LispVal IO

liftThrows :: MonadError e m => Either e a -> m a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

-- runIOThrows :: IOThrowsError String -> IO String
runIOThrows :: Monad f => ErrorT LispError f String -> f String
runIOThrows action = extractValue <$> runErrorT (trapError action)

-- isBound :: Env -> String -> IO Bool
isBound :: Eq a => IORef [(a, b)] -> a -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef

nullEnv = newIORef []
