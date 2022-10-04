module Repl where

import System.IO (hFlush, stdout)

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

readPrompt :: String -> IO String
readPrompt p = flushStr p >> getLine


