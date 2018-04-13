{-# LANGUAGE QuasiQuotes #-}

import Test.HUnit
import Text.RawString.QQ
import GHC.Real (Ratio((:%)))
import Eval
import Value
import Parse
import Parser
import Num
import Prim


(****?=) :: String -> ScmVal -> Test
str ****?= expected = TestCase $
  case runParser parse str of
    Fail err -> assertFailure err
    Succeed (expr, _) -> do
      env <- initialEnv
      actual <- runEval $ eval env expr
      assertEqual str expected actual

list :: [ScmVal] -> ScmVal
list = fromHsList

dlist :: [ScmVal] -> ScmVal -> ScmVal
dlist [x] y = VCons x y
dlist (x:xs) y = VCons x $ dlist xs y
dlist _ _ = error "unreachable"


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
              VNum 123
        , [r|
          'xyz-ABC
          |]
            ****?=
              VSym "xyz-ABC"
        , [r|
          '(a #t)
          |]
            ****?=
              list [VSym "a", VTrue]
        , [r|
          '()
          |]
            ****?=
              VNil
        , [r|
          (quote 123)
          |]
            ****?=
              VNum 123
        ]
  , "eval function app" ~:
      TestList
        [
          [r|
          (cons 1 (cons 2 '()))
          |]
            ****?=
              list [VNum 1, VNum 2]
        , [r|
          (+ 3 5 6)
          |]
            ****?=
              VNum 14
        , [r|
          ((lambda (x) (+ x 3)) 5)
          |]
            ****?=
              VNum 8
        , [r|
          ((lambda (x y) (cons y x)) 5 6)
          |]
            ****?=
              VCons (VNum 6) (VNum 5)
        , [r|
          ((lambda x x) 1 3)
          |]
            ****?=
              list [VNum 1, VNum 3]
        , [r|
          ((lambda (x . y) (cons y x)) 1 2 3)
          |]
            ****?=
              VCons (list [VNum 2, VNum 3]) (VNum 1)
        , [r|
          (apply + '(1 2 3 4))
          |]
            ****?=
              VNum 10
        , [r|
          (apply + 1 2 '(3 4 5))
          |]
            ****?=
              VNum 15
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
              VNum 123
        , [r|
          (begin
            (define f
              (lambda (x) (+ x 4)))
            (f 5))
          |]
            ****?=
              VNum 9
        , [r|
          (begin
            (define (f x y)
              (- y x))
            (f 2 7))
          |]
            ****?=
              VNum 5
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
              VCons (VNum 8) (VNum 1)
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
              VNum 120
        , [r|
          (begin
            (define (f n)
              (if (< n 2)
                  n
                  (+ (f (- n 1)) (f (- n 2)))))
            (define (map f xs)
              (if (null? xs)
                  '()
                  (cons (f (car xs))
                        (map f (cdr xs)))))
            (map f '(0 1 2 3 4 5 6 7 8 9 10 11)))
          |]
            ****?=
              list (map VNum [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89])
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
  , "eval quasiquote" ~:
      TestList
        [
          [r|
          `(list ,(+ 1 2) 4)
          |]
            ****?=
              list [VSym "list", VNum 3, VNum 4]
        , [r|
          `(,@(list 1 2) 3)
          |]
            ****?=
              list [VNum 1, VNum 2, VNum 3]
        , [r|
          `(1 `(2 ,(a ,(+ 3 1) ,@(cons 5 (cons 6 '())) . b)))
          |]
            ****?=
              list [VNum 1, list [VSym "quasiquote", list [VNum 2, list [VSym "unquote", dlist [VSym "a", VNum 4, VNum 5, VNum 6] (VSym "b")]]]]
        ]
  , "eval macro" ~:
      TestList
        [
          [r|
          (begin
            (defmacro (let bindings . body)
              `((lambda ,(map car bindings)
                  . ,body)
                . ,(map cadr bindings)))

            (define (map f xs)
              (if (null? xs)
                  '()
                  (cons (f (car xs))
                        (map f (cdr xs)))))

            (define (cadr x) (car (cdr x)))

            (let ([x 1] [y 2])
              (+ x y)))
          |]
            ****?=
              VNum 3
        ]
  ]

main :: IO ()
main = () <$ runTestTT tests