module Main where

import Etor
import Parser (readExpr)
import Repl (evalAndPrint, runOne, runRepl)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> runOne $ head args
    _ -> putStrLn "Takes 0 or 1 args"