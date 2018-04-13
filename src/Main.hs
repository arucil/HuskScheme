{-# LANGUAGE Strict #-}

module Main where

import Parse
import Parser (Parser(..), Result(..))
import Error (ScmError(CustomError))
import Eval
import Prim
import System.Exit (exitSuccess)
import Control.Monad.IO.Class
import System.Console.ANSI
import System.Console.Haskeline

repl :: IO ()
repl = do
  env <- initialEnv
  _   <- runEval $ loadFile env "prelude/prelude.scm"

  let
    loop :: InputT IO ()
    loop = do
      line <- getInputLine "> "
      case line of
        Nothing -> liftIO $ exitSuccess
        Just "" -> loop
        Just line' ->
            case runParser parse line' of
              Fail err -> throwIO $ CustomError $ "parse error: " ++ err
              Succeed (expr, _) -> do
                val <- liftIO $ runEval $ eval env expr
                liftIO $ print val
                loop
          `catch` (((>> loop) . liftIO) . errorHandler)

    errorHandler :: ScmError -> IO ()
    errorHandler e = do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ "Error: " ++ show e
      setSGR [Reset]

  runInputT defaultSettings loop

main :: IO ()
main = repl
