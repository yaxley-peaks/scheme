module Main where

import Etor
import Parser (readExpr)
import Repl (evalAndPrint, runOne, runRepl)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args then runRepl else runOne args