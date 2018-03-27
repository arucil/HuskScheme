{-# LANGUAGE QuasiQuotes #-}

import Test.HUnit
import Text.RawString.QQ
import Eval
import Value
import Parse
import Parser
import GHC.Real (Ratio((:%)))


(****?=) :: String -> ScmVal -> Test
str ****?= expected = TestCase $
  case runParser parse str of
    Fail err -> assertFailure err
    Succeed (expr, _) -> do
      actual <- initialEnv >>= (`eval` expr)
      assertEqual str expected actual


tests :: Test
tests = test
  [
    "eval atom" ~:
      TestList
        [
          [r|
          #t
          |]
            ****?=
              VTrue
        , [r|
          #\=
          |]
            ****?=
              VChar '='
        , [r|
          "abc\""
          |]
            ****?=
              VStr "abc\""
        ]
  , "eval quote" ~:
      TestList
        [
          [r|
          '123
          |]
            ****?=
              VNum (ScmNum 123)
        , [r|
          'xyz-ABC
          |]
            ****?=
              VSym "xyz-ABC"
        , [r|
          '(a #t)
          |]
            ****?=
              VCons (VSym "a")
                    (VCons VTrue VNil)
        , [r|
          '()
          |]
            ****?=
              VNil
        ]
  , "eval function app" ~:
      TestList
        [
          [r|
          (cons 1 2)
          |]
            ****?=
              VCons (VNum $ ScmNum 1)
                    (VNum $ ScmNum 2)
        , [r|
          (+ 3 5 6)
          |]
            ****?=
              VNum (ScmNum 14)
        , [r|
          ((lambda (x) (+ x 3)) 5)
          |]
            ****?=
              VNum (ScmNum 8)
        , [r|
          ((lambda (x y) (cons y x)) 5 6)
          |]
            ****?=
              VCons (VNum $ ScmNum 6)
                    (VNum $ ScmNum 5)
        , [r|
          ((lambda x x) 1 3)
          |]
            ****?=
              VCons (VNum $ ScmNum 1)
                    (VCons (VNum $ ScmNum 3)
                           VNil)
        , [r|
          ((lambda (x . y) (cons y x)) 1 2 3)
          |]
            ****?=
              VCons (VCons (VNum $ ScmNum 2)
                           (VCons (VNum $ ScmNum 3)
                                  VNil))
                    (VNum $ ScmNum 1)
        ]
  , "eval define" ~:
      TestList
        [
          [r|
          (begin
            (define x 123)
            x)
          |]
            ****?=
              VNum (ScmNum 123)
        , [r|
          (begin
            (define f
              (lambda (x) (+ x 4)))
            (f 5))
          |]
            ****?=
              VNum (ScmNum 9)
        , [r|
          (begin
            (define (f x y)
              (- y x))
            (f 2 7))
          |]
            ****?=
              VNum (ScmNum 5)
        ]
  , "eval set!" ~:
      TestList
        [
          [r|
          (begin
            (define x 0)
            (define y 1)
            (set! x (+ y 7))
            (cons x y))
          |]
            ****?=
              VCons (VNum $ ScmNum 8)
                    (VNum $ ScmNum 1)
        ]
  , "eval recursion" ~:
      TestList
        [
          [r|
          (begin
            (define (f n)
              (if (< n 1)
                1
                (* n (f (- n 1)))))
            (f 5))
          |]
            ****?=
              VNum (ScmNum 120)
        ]
  , "eval let" ~:
      TestList
        [
          [r|
          (let () 12)
          |]
            ****?=
              VNum (ScmNum 12)
        , [r|
          (let ([x 1] [y 2])
            (cons x y))
          |]
            ****?=
              VCons (VNum $ ScmNum 1)
                    (VNum $ ScmNum 2)
        ]
  , "eval rationals" ~:
      TestList
        [
          [r|
          (+ 3/5 -1/5)
          |]
            ****?=
              VNum (ScmNum $ 2 :% 5)
        , [r|
          (- 6/14)
          |]
            ****?=
              VNum (ScmNum $ (-3) :% 7)
        , [r|
          (* 3/5 5)
          |]
            ****?=
              VNum (ScmNum $ 3 :% 1)
        , [r|
          (/ 1/2 7/3)
          |]
            ****?=
              VNum (ScmNum $ 3 :% 14)
        , [r|
          (/ 7/3)
          |]
            ****?=
              VNum (ScmNum $ 3 :% 7)
        , [r|
          (/ 7 3)
          |]
            ****?=
              VNum (ScmNum $ 7 :% 3)
        ]
  ]

main :: IO ()
main = () <$ runTestTT tests