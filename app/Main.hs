module Main where

import Parser (readExpr)
import System.Environment (getArgs)
import Etor

main :: IO ()
main = getArgs >>= print . eval . readExpr . (!! 0)