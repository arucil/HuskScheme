-- {-# LANGUAGE Strict #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Parse
import Parser (Parser(..), Result(..))
import Value (ScmError)
import Eval
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
  , [r|
    (define (Y f)
      ((lambda (g) (g g))
       (lambda (g)
         (lambda (x)
           ((f (g g)) x)))))
    |]
  ]


repl :: IO ()
repl = do
  env <- initialEnv
  let
    prepare e =
      case runParser parse e of
        Fail err -> error err
        Succeed (expr, _) -> eval env expr

    loop :: IO ()
    loop = do
      putStr "> "
      hFlush stdout
      eof <- isEOF
      when eof $
        exitSuccess
      line <- getLine
      when (null line) $
        loop
      case runParser parse line of
        Fail err -> do
          putStrLn $ "Parse error: " ++ err
          loop
        Succeed (expr, _) -> (eval env expr >>= print >> loop)
          `catch` errorHandler

    errorHandler :: ScmError -> IO ()
    errorHandler e = do
      putStrLn $ "!!! Error: " ++ show e
      loop

  mapM_ prepare prelude
  loop



main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  repl
