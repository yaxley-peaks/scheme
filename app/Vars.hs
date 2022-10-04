module Vars where

import Data.IORef
import Data
type Env = [(String, IORef LispVal)]
