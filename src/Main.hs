{-# LANGUAGE Strict #-}

module Main where

import Parse
import Parser (Parser(..), Result(..))
import Value (ScmError(CustomError))
import Eval
import System.Exit (exitSuccess)
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Exception
import System.Console.ANSI
import System.Console.Haskeline hiding (catch, throwIO)

repl :: IO ()
repl = do
  env <- initialEnv
  let
    loop :: InputT IO ()
    loop = do
      line <- getInputLine "> "
      case line of
        Nothing -> liftIO $ exitSuccess
        Just "" -> loop
        Just line' -> do
          liftIO $
            (case runParser parse line' of
              Fail err -> throwIO $ CustomError $ "parse error: " ++ err
              Succeed (expr, _) -> eval env expr >>= print)
              `catch` errorHandler
          loop

    errorHandler :: ScmError -> IO ()
    errorHandler e = do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ "Error: " ++ show e
      setSGR [Reset]

  loadFile env "prelude/prelude.scm"
  runInputT defaultSettings loop

main :: IO ()
main = repl
