module Parser where

import Control.Monad (liftM)
import Data (LispVal (..))
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!$ %&|*+ -/: <=? > @^_ ~#"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString =
  let escapedChars = do
        char '\\'
        x <- oneOf "\\\"ntr"
        case x of
          '\\' -> do return [x]
          '"' -> do return [x]
          't' -> do return "\t"
          'n' -> do return "\n"
          'r' -> do return "\r"
          _ -> do return ""
   in do
        char '"'
        x <- many (noneOf "\"\\") <|> escapedChars
        char '"'
        return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of --lisp is just the name
  Left err -> "No Match!: " ++ show err
  Right val -> "Found value"