module Main where

import Parser (readExpr)
import System.Environment (getArgs)

main = do
  (arg:_) <- getArgs
  putStrLn (readExpr arg)