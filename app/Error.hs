{-# OPTIONS_GHC -Wno-deprecations #-}

module Error where

import Control.Monad.Error
import Data (LispVal)
import Text.Parsec (ParseError)

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

