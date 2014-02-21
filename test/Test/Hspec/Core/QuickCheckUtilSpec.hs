module Test.Hspec.Core.QuickCheckUtilSpec (main, spec) where

import           Helper

import           Test.Hspec.Core.QuickCheckUtil

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "integerToSeed" $ do
    it "is inverse to seedToInteger" $ do
      property $ \n -> (integerToSeed . seedToInteger) n `shouldBe` n
