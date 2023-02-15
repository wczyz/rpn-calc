module RpnSpec where

import Test.Hspec
import Calc

inputs :: [(String, Either Err Int)]
inputs = 
  [ ("2 5 +", Right 7)
  , ("5 14 - 2 * 3 +", Right (-15))
  , ("1 2 3 4 5 6 7 8", Right 8)
  , ("5 13 2 / 20 - negate", Right 14)
  , ("d", Left (ParseError "d"))
  , ("2 2 + ]", Left (ParseError "]"))
  , ("2 3 + 14 - 3 2 1 / divide negate", Left (ParseError "divide"))
  , ("-", Left (StackSizeError 2 0))
  , ("negate", Left (StackSizeError 1 0))
  , ("2 5 * negate +", Left (StackSizeError 2 1))
  ]

spec :: Spec
spec = do
  describe "RPN calculator tests" $
    mapM_
    (\(input, expectedOutput) ->
      it "" $
        calculate input
        `shouldBe`
        expectedOutput
    )
    inputs
