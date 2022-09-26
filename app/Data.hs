-- {-# LANGUAGE StrictData #-}
module Data where

-- import Etor (showVal)

data LispVal
  = Atom !String
  | List ![LispVal]
  | DottedList ![LispVal] !LispVal
  | Number !Integer
  | String !String
  | Bool !Bool

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number content) = show content
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ "." ++ showVal tail

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal