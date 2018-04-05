{-# LANGUAGE QuasiQuotes #-}

module Main where

import Test.HUnit
import Text.RawString.QQ
import Parse
import Parser
import Value
import Data.Ratio ((%))

tests :: Test
tests = test
  [
    "parse number" ~:
      TestList
        [
          p "123" ~?= valid (VNum $ ScmNum 123)
        , p "  4" ~?= valid (VNum $ ScmNum 4)
        , p "  +4" ~?= valid (VNum $ ScmNum 4)
        , p "-13   " ~?= valid (VNum $ ScmNum (-13))
        , p "    0013  " ~?= valid (VNum $ ScmNum 13)
        , p "    13/14  " ~?= valid (VNum $ ScmNum $ 13 % 14)
        , p "    -13/14  " ~?= valid (VNum $ ScmNum $ (-13) % 14)
        , p "    +13/14  " ~?= valid (VNum $ ScmNum $ 13 % 14)
        ]
  , "parse symbol" ~:
      TestList
        [
          p "abc" ~?= valid (VSym "abc")
        , p "r" ~?= valid (VSym "r")
        , p "AbC" ~?= valid (VSym "AbC")
        , p "abc12_" ~?= valid (VSym "abc12_")
        , p "an-ident-Slash" ~?= valid (VSym "an-ident-Slash")
        , p "++" ~?= valid (VSym "++")
        , p "1+" ~?= valid (VSym "1+")
        , p "some-pred?" ~?= valid (VSym "some-pred?")
        , p "1.5" ~?= valid (VSym "1.5")
        ]
  , "parse boolean" ~:
      TestList
        [
          p "#t" ~?= valid VTrue
        , p "   #f  " ~?= valid VFalse
        ]
  , "parse character" ~:
      TestList
        [
          p "#\\A" ~?= valid (VChar 'A')
        , p "#\\a" ~?= valid (VChar 'a')
        , p "#\\(" ~?= valid (VChar '(')
        , p "#\\#" ~?= valid (VChar '#')
        , p "#\\ " ~?= valid (VChar ' ')
        , p "#\\\n" ~?= valid (VChar '\n')
        , p "#\\newline" ~?= valid (VChar '\n')
        , p "#\\space" ~?= valid (VChar ' ')
        ]
  , "parse string" ~:
      TestList
        [
          p [r| "" |] ~?= valid (VStr "")
        , p [r| "A" |] ~?= valid (VStr "A")
        , p [r| "abcdefg\n356" |] ~?= valid (VStr "abcdefg\n356")
        , p [r| "\rab\tcd\"efg\n356" |] ~?= valid (VStr "\rab\tcd\"efg\n356")
        , p [r| "abcdefg
356" |] ~?= valid (VStr "abcdefg\n356")
        ]
  , "parse list" ~:
      TestList
        [
          p "()" ~?= valid VNil
        , p "(#t)" ~?= valid (VCons VTrue VNil)
        , p [r| (13 "Str") |] ~?= valid (VCons (VNum $ ScmNum 13)
                                               (VCons (VStr "Str")
                                                      VNil))
        , p [r| (+ 3 (* 5 -1 7)) |] ~?= valid (VCons (VSym "+")
                                                     (VCons (VNum $ ScmNum 3)
                                                            (VCons (VCons (VSym "*")
                                                                          (VCons (VNum $ ScmNum 5)
                                                                                 (VCons (VNum $ ScmNum (-1))
                                                                                        (VCons (VNum $ ScmNum 7) VNil))))
                                                                   VNil)))
        , p [r| (dotted . pair) |] ~?= valid (VCons (VSym "dotted") (VSym "pair"))
        , p [r| (a dotted . list) |] ~?= valid (VCons (VSym "a")
                                                      (VCons (VSym "dotted")
                                                             (VSym "list")))
        ]
  , "parentheses and brackets" ~:
      TestList
        [
          p "[]" ~?= valid VNil
        , p "[1]" ~?= valid (VCons (VNum $ ScmNum 1) VNil)
        , p "[(#t) []]" ~?= valid (VCons (VCons VTrue VNil) (VCons VNil VNil))
        ]
  , "parse quote" ~:
      let q x = VCons (VSym "quote") (VCons x VNil)
      in
        TestList
          [
            p "'-123" ~?= valid (q $ VNum $ ScmNum (-123))
          , p "'a" ~?= valid (q $ VSym "a")
          , p "'#t" ~?= valid (q VTrue)
          , p "'()" ~?= valid (q VNil)
          , p "'(a b)" ~?= valid (q $ VCons (VSym "a") (VCons (VSym "b") VNil))
          , p "''x" ~?= valid (q $ q $ VSym "x")
          ]
  , "parse quasiquote" ~:
      let qq x = VCons (VSym "quasiquote") (VCons x VNil)
          u  x = VCons (VSym "unquote") (VCons x VNil)
          us x = VCons (VSym "unquote-splicing") (VCons x VNil)
      in
        TestList
          [
            p "`x" ~?= valid (qq $ VSym "x")
          , p "`,(+)" ~?= valid (qq $ u $ VCons (VSym "+") VNil)
          , p ",@123" ~=? valid (us $ VNum 123)
          , p "`(,,12 ,@())" ~?= valid (qq $ VCons (u $ u $ VNum 12)
                                                   (VCons (us VNil) VNil))
          ]
  , "parse a list of expressions" ~:
      TestList
        [
          pl "" ~?= valid []
        , pl ";123\n\t" ~?= valid []
        , pl "(+ 1 2) hEllo" ~?= valid [ VCons (VSym "+")
                                               (VCons (VNum 1)
                                                      (VCons (VNum 2)
                                                             VNil))
                                       , VSym "hEllo" ]
        , pl "123   #t \t\n []" ~?= valid [ VNum 123, VTrue, VNil ]
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
        , p "(" ~?= invalid "invalid token EOF"
        , p ")" ~?= invalid "invalid token RParen"
        , p "(1" ~?= invalid "invalid token EOF"
        , p "'" ~?= invalid "invalid token EOF"
        , p "(]" ~?= invalid "invalid token RBrack"
        , p "[)" ~?= invalid "invalid token RParen"
        , p "(1 . 2 3)" ~?= invalid "expected: RParen, got: TokNum 3"
        , p "(1 .)" ~?= invalid "invalid token RParen"
        ]
  ]
  where
    valid res = Succeed (res, "")
    invalid = Fail
    p = runParser parse
    pl = runParser parseList

main :: IO ()
main = () <$ runTestTT tests