module RpnSpec where

import Test.Hspec
import Calc (calculate)

inputs :: [(String, Int)]
inputs = 
  [ ("2 5 +", 7)
  , ("5 14 - 2 * 3 +", -15)
  ]

spec :: Spec
spec = do
  describe "RPN calculator tests" $ do
    it "" $ 
      shouldBe 
      (map (calculate . fst) inputs) 
      (map snd inputs)
