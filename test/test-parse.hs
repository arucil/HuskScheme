module Main where

import Test.Hspec
import Parse
import Parser
import Value

main :: IO ()
main = hspec $ do
  let valid r =  Succeed (r, "")
      p = runParser parse
  describe "Parse Atom" $
    it "Numbers" $ do
      p "123" `shouldBe` valid (VNum $ ScmNum 123)
      p "  4  " `shouldBe` valid (VNum $ ScmNum 4)