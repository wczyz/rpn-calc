module Main where

data Token
  = Value Int
  | Plus
  | Minus
  | Times
  | Divide
  | Negate

tokenize :: String -> [Token]
tokenize input = map toToken $ words input
  where
    toToken :: String -> Token
    toToken s = case s of
      "+" -> Plus
      "-" -> Minus
      "*" -> Times
      "/" -> Divide
      "negate" -> Negate
      _ -> Value (read s)

evaluate :: [Token] -> Int
evaluate tokens = head $ foldl runToken [] tokens
  where 
    stackBinaryOp :: [Int] -> (Int -> Int -> Int) -> [Int]
    stackBinaryOp (x : y : rest) f = f y x : rest

    stackUnaryOp :: [Int] -> (Int -> Int) -> [Int]
    stackUnaryOp (x : rest) f = f x : rest

    runToken :: [Int] -> Token -> [Int]
    runToken stack token =
      case token of
        Value x -> x : stack
        Plus -> stackBinaryOp stack (+)
        Minus -> stackBinaryOp stack (-)
        Times -> stackBinaryOp stack (*)
        Divide -> stackBinaryOp stack div
        Negate -> stackUnaryOp stack negate

calculate :: String -> Int
calculate = evaluate . tokenize

main :: IO ()
main = do
  input <- getContents
  mapM_ (print . calculate) (lines input)
