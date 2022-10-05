{-# OPTIONS_GHC -Wno-deprecations #-}

-- {-# LANGUAGE StrictData #-}
module Data where

import Control.Monad.Error
  ( Error (..),
    ErrorT (runErrorT),
    MonadError (..),
    MonadIO (liftIO),
  )
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Text.Parsec (ParseError)

data LispVal
  = Atom !String
  | List ![LispVal]
  | DottedList ![LispVal] !LispVal
  | Number !Integer
  | String !String
  | Bool !Bool
  | PrimitiveFunc !([LispVal] -> ThrowsError LispVal)
  | Func
      { params :: ![String],
        vararg :: !(Maybe String),
        body :: ![LispVal],
        closure :: !Env
      }

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number content) = show content
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ "." ++ showVal tail
showVal (PrimitiveFunc _) = "<primitive>"
showVal
  Func
    { params = args,
      vararg = varargs,
      body = body,
      closure = env
    } =
    "(lambda (" ++ unwords (map show args)
      ++ ( case varargs of
             Nothing -> ""
             Just arg -> " . " ++ arg
         )
      ++ ")... )"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected
    ++ " args; found values "
    ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected
    ++ ", found "
    ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError _ = undefined

type ThrowsError = Either LispError

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left val) = error "Unreachable!"

instance Show LispError where show = showError

instance Error LispError where
  noMsg = Default "An error has occured"
  strMsg = Default

type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ErrorT LispError IO

liftThrows :: MonadError e m => Either e a -> m a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runErrorT (trapError action)

isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . flip writeIORef value)
    (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alDef'd <- liftIO $ isBound envRef var
  if alDef'd
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = (++ env) <$> mapM addBinding bindings
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)

nullEnv :: IO Env
nullEnv = newIORef []
