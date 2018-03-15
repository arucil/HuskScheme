{-# LANGUAGE Strict #-}

module Main where

import Parse
import Parser (Parser(..), Result(..))
import Value (ScmVal(..), ScmPrim(..), Env, Store, ScmError)
import StateT (StateT(..))
import Eval
import System.Console.ANSI
import System.IO
import System.Exit (exitSuccess)
import Control.Monad (when)
import Control.Exception


repl :: IO ()
repl = loop (initialEnv, initialStore)
  where
    loop :: St -> IO ()
    loop st = do
      eof <- isEOF
      when eof $
        exitSuccess
      putStr "> "
      hFlush stdout
      line <- getLine
      when (null line) $
        loop st
      case runParser parse line of
        Fail err -> do
          putStrLn $ "Parse error: " ++ err
          loop st
        Succeed (expr, _) -> (do
            (val, st') <- eval expr st
            print val
            loop st')
          `catch` errorHandler

    errorHandler :: ScmError -> IO ()
    errorHandler e = putStrLn $ "### Error: " ++ show e



main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  repl
