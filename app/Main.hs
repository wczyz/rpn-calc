module Main where

import Calc (calculate)

main :: IO ()
main = do
  input <- getContents
  mapM_ (print . calculate) (lines input)
