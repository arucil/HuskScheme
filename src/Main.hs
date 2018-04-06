{-# LANGUAGE Strict #-}

module Main where

import Parse
import Parser (Parser(..), Result(..))
import Value (FakePtr)
import Error (ScmError(CustomError))
import Eval
import Prim
import StateT
import System.Exit (exitSuccess)
import Control.Monad.IO.Class
import System.Console.ANSI
import System.Console.Haskeline

repl :: IO ()
repl = do
  (env, ptr0) <- runStateT initialEnv 0
  (_, ptr1) <- runStateT (loadFile env "prelude/prelude.scm") ptr0

  let
    loop :: FakePtr -> InputT IO ()
    loop ptr = do
      line <- getInputLine "> "
      case line of
        Nothing -> liftIO $ exitSuccess
        Just "" -> loop ptr
        Just line' ->
            case runParser parse line' of
              Fail err -> throwIO $ CustomError $ "parse error: " ++ err
              Succeed (expr, _) -> do
                (val, ptr') <- liftIO $ runStateT (eval env expr) ptr
                liftIO $ print val
                loop ptr'
          `catch` (((>> loop ptr) . liftIO) . errorHandler)

    errorHandler :: ScmError -> IO ()
    errorHandler e = do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ "Error: " ++ show e
      setSGR [Reset]

  runInputT defaultSettings $ loop ptr1

main :: IO ()
main = repl
