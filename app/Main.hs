module Main where

import Calc (calculate)

main :: IO ()
main = do
  input <- getContents

  let handleLine :: String -> IO ()
      handleLine line =
        case calculate line of
          Right result -> print result
          Left err -> print err

  mapM_ handleLine (lines input)
