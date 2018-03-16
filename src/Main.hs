{-# LANGUAGE Strict #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Parse
import Parser (Parser(..), Result(..))
import Value (ScmError)
import Eval
import StateT
import System.IO
import System.Exit (exitSuccess)
import Control.Monad (when)
import Control.Exception
import Text.RawString.QQ


prelude :: [String]
prelude =
  [
    [r|
    (define (not x)
      (if x #f #t))
    |]
  , [r|
    (define (list . xs) xs)
    |]
  , [r|
    (define (map f xs)
      (if (null? xs)
        '()
        (cons (f (car xs))
              (map f (cdr xs)))))
    |]
  ]


repl :: IO ()
repl = do
  let prepare e =
        case runParser parse e of
          Fail err -> error err
          Succeed (expr, _) -> evalExpr expr
  (_, st0) <- runStateT (mapM prepare prelude) (initialEnv, initialStore)
  loop st0
  where
    loop :: St -> IO ()
    loop st = do
      putStr "> "
      hFlush stdout
      eof <- isEOF
      when eof $
        exitSuccess
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
          `catch` errorHandler st

    errorHandler :: St -> ScmError -> IO ()
    errorHandler st e = do
      putStrLn $ "!!! Error: " ++ show e
      loop st



main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  repl
