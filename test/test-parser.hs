
module Main where

import Parser
import Test.QuickCheck (Arbitrary(..), frequency)
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers


instance Arbitrary a => Arbitrary (Result a) where
  arbitrary = frequency [(1, Fail <$> arbitrary), (3, Succeed <$> arbitrary)]


instance Eq a => EqProp (Result a) where
  (=-=) = eq


resultInstances :: IO ()
resultInstances = do
  putStrLn " Result instances:"
  quickBatch $ functor $ (undefined :: Result (Int, String, Integer))
  quickBatch $ applicative $ (undefined :: Result (Int, String, Integer))
  quickBatch $ monad $ (undefined :: Result (Int, Integer, String))


main :: IO ()
main = do
  resultInstances


--- Don't know how to quickcheck functor/applicative/monad laws for StateT-like
--- datatypes ... so the tests for Parser are left out.