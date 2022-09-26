module Main where

import Parser (readExpr)
import System.Environment (getArgs)

main = do
  args <- getArgs
  putStrLn (readExpr (head args))