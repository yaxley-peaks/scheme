module Main where

import Parser (readExpr)
import System.Environment (getArgs)
import Etor
import Error (extractValue, trapError)

main :: IO ()
main = do
     args <- getArgs
     let evaled = fmap show $ readExpr (head args) >>= eval
     putStrLn $ extractValue $ trapError evaled