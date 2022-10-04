{-# OPTIONS_GHC -Wno-deprecations #-}

module Vars where

import Control.Monad.Error (ErrorT (runErrorT), MonadError (throwError))
import Data ( LispVal )
import Data.IORef ( IORef, newIORef )
import Error (LispError, ThrowsError, extractValue, trapError)

type Env = [(String, IORef LispVal)]

type IOThrowsError = ErrorT LispVal IO

liftThrows :: MonadError e m => Either e a -> m a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

-- runIOThrows :: IOThrowsError String -> IO String
runIOThrows :: Monad f => ErrorT LispError f String -> f String
runIOThrows action = extractValue <$> runErrorT (trapError action)

nullEnv = newIORef []
