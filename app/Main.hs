module Main where

import Error (extractValue, trapError)
import Etor
import Parser (readExpr)
import Repl (evalAndPrint, runRepl)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> evalAndPrint $ head args
    _ -> putStrLn "Takes 0 or 1 args"