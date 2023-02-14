module RpnSpec where

import Test.Hspec
import Calc (calculate)

inputs :: [(String, Int)]
inputs = 
  [ ("2 5 +", 7)
  , ("5 14 - 2 * 3 +", -15)
  , ("1 2 3 4 5 6 7 8", 8)
  , ("5 13 2 / 20 - negate", 14)
  ]

spec :: Spec
spec = do
  describe "RPN calculator tests" $ do
    it "" $ 
      map (calculate . fst) inputs
      `shouldBe`
      map snd inputs
