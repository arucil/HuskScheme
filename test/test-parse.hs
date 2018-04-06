{-# LANGUAGE QuasiQuotes #-}

module Main where

import Test.HUnit
import Text.RawString.QQ
import Parse
import Parser
import Program
import Num (ScmNum(..))
import Data.Ratio ((%))

tests :: Test
tests = test
  [
    "parse number" ~:
      TestList
        [
          p "123" ~?= valid (PNum $ ScmNum 123)
        , p "  4" ~?= valid (PNum $ ScmNum 4)
        , p "  +4" ~?= valid (PNum $ ScmNum 4)
        , p "-13   " ~?= valid (PNum $ ScmNum (-13))
        , p "    0013  " ~?= valid (PNum $ ScmNum 13)
        , p "    13/14  " ~?= valid (PNum $ ScmNum $ 13 % 14)
        , p "    -13/14  " ~?= valid (PNum $ ScmNum $ (-13) % 14)
        , p "    +13/14  " ~?= valid (PNum $ ScmNum $ 13 % 14)
        ]
  , "parse symbol" ~:
      TestList
        [
          p "abc" ~?= valid (PSym "abc")
        , p "r" ~?= valid (PSym "r")
        , p "AbC" ~?= valid (PSym "AbC")
        , p "abc12_" ~?= valid (PSym "abc12_")
        , p "an-ident-Slash" ~?= valid (PSym "an-ident-Slash")
        , p "++" ~?= valid (PSym "++")
        , p "1+" ~?= valid (PSym "1+")
        , p "some-pred?" ~?= valid (PSym "some-pred?")
        , p "1.5" ~?= valid (PSym "1.5")
        ]
  , "parse boolean" ~:
      TestList
        [
          p "#t" ~?= valid PTrue
        , p "   #f  " ~?= valid PFalse
        ]
  , "parse character" ~:
      TestList
        [
          p "#\\A" ~?= valid (PChar 'A')
        , p "#\\a" ~?= valid (PChar 'a')
        , p "#\\(" ~?= valid (PChar '(')
        , p "#\\#" ~?= valid (PChar '#')
        , p "#\\ " ~?= valid (PChar ' ')
        , p "#\\\n" ~?= valid (PChar '\n')
        , p "#\\newline" ~?= valid (PChar '\n')
        , p "#\\space" ~?= valid (PChar ' ')
        ]
  , "parse string" ~:
      TestList
        [
          p [r| "" |] ~?= valid (PStr "")
        , p [r| "A" |] ~?= valid (PStr "A")
        , p [r| "abcdefg\n356" |] ~?= valid (PStr "abcdefg\n356")
        , p [r| "\rab\tcd\"efg\n356" |] ~?= valid (PStr "\rab\tcd\"efg\n356")
        , p [r| "abcdefg
356" |] ~?= valid (PStr "abcdefg\n356")
        ]
  , "parse list" ~:
      TestList
        [
          p "()" ~?= valid (PList [])
        , p "(#t)" ~?= valid (PList [PTrue])
        , p [r| (13 "Str") |] ~?= valid (PList [PNum 13, PStr "Str"])
        , p [r| (+ 3 (* 5 -1 7)) |] ~?= valid (PList [PSym "+", PNum 3, PList [PSym "*", PNum 5, PNum (-1), PNum 7]])
        , p [r| (dotted . pair) |] ~?= valid (PDList [PSym "dotted"] (PSym "pair"))
        , p [r| (a dotted . list) |] ~?= valid (PDList [PSym "a", PSym "dotted"] (PSym "list"))
        ]
  , "parentheses and brackets" ~:
      TestList
        [
          p "[]" ~?= valid (PList [])
        , p "[1]" ~?= valid (PList [PNum 1])
        , p "[(#t) []]" ~?= valid (PList [PList [PTrue], PList []])
        ]
  , "parse quote" ~:
      let q x = PList [PSym "quote", x]
      in
        TestList
          [
            p "'-123" ~?= valid (q $ PNum (-123))
          , p "'a" ~?= valid (q $ PSym "a")
          , p "'#t" ~?= valid (q PTrue)
          , p "'()" ~?= valid (q $ PList [])
          , p "'(a b)" ~?= valid (q $ PList [PSym "a", PSym "b"])
          , p "''x" ~?= valid (q $ q $ PSym "x")
          ]
  , "parse quasiquote" ~:
      let qq x = PList [PSym "quasiquote", x]
          u  x = PList [PSym "unquote", x]
          us x = PList [PSym "unquote-splicing", x]
      in
        TestList
          [
            p "`x" ~?= valid (qq $ PSym "x")
          , p "`,(+)" ~?= valid (qq $ u $ PList [PSym "+"])
          , p ",@123" ~=? valid (us $ PNum 123)
          , p "`(,,12 ,@())" ~?= valid (qq $ PList [u $ u $ PNum 12, us $ PList []])
          ]
  , "parse a list of expressions" ~:
      TestList
        [
          pl "" ~?= valid []
        , pl ";123\n\t" ~?= valid []
        , pl "(+ 1 2) hEllo" ~?= valid [ PList [PSym "+", PNum 1, PNum 2]
                                       , PSym "hEllo" ]
        , pl "123   #t \t\n []" ~?= valid [ PNum 123, PTrue, PList [] ]
        , pl "(a) b )" ~?= invalid "expected: EOF, got: RParen"
        ]
  , "invalid" ~:
      TestList
        [
          p "" ~?= invalid "invalid token EOF"
        , p "#T" ~?= invalid "invalid token #T"
        , p "#true" ~?= invalid "invalid token #true"
        , p "#\\" ~?= invalid "expected: a char, got: EOF"
        , p "#\\how" ~?= invalid "invalid character #\\how"
        , p [r|"|] ~?= invalid "expected: a char, got: EOF"
        , p [r| "he |] ~?= invalid "expected: a char, got: EOF"
        , p [r| "\k" |] ~?= invalid "invalid escape sequence \\k"
        , p "(" ~?= invalid "expected: RParen, got: EOF"
        , p ")" ~?= invalid "invalid token RParen"
        , p "(1" ~?= invalid "expected: RParen, got: EOF"
        , p "'" ~?= invalid "invalid token EOF"
        , p "(]" ~?= invalid "expected: RParen, got: RBrack"
        , p "[)" ~?= invalid "expected: RBrack, got: RParen"
        , p "(1 . 2 3)" ~?= invalid "expected: RParen, got: TokNum 3"
        , p "(1 .)" ~?= invalid "invalid token RParen"
        , p "(. 1)" ~?= invalid "expected: RParen, got: Dot"
        ]
  ]
  where
    valid res = Succeed (res, "")
    invalid = Fail
    p = runParser parse
    pl = runParser parseList

main :: IO ()
main = () <$ runTestTT tests