module Calc.Internal where

import Text.Read (readMaybe)
import Control.Monad (foldM)
import Control.Monad.Trans.Except

data Token
  = Value Int
  | Plus
  | Minus
  | Times
  | Divide
  | Negate

data Err
  = ParseError String
  | StackSizeError Int Int
  deriving (Eq)

instance Show Err where
  show (ParseError s) = "Could not parse " ++ s ++ " into a valid token"
  show (StackSizeError expectedAtLeast got) = 
    "Stack too small: expected at least " ++ show expectedAtLeast 
    ++ " elements, but got " ++ show got

tokenize :: String -> Except Err [Token]
tokenize input = mapM toToken $ words input
  where
    toToken :: String -> Except Err Token
    toToken s = case s of
      "+" -> return Plus
      "-" -> return Minus
      "*" -> return Times
      "/" -> return Divide
      "negate" -> return Negate
      _ -> 
        case readMaybe s of
          Just v -> return (Value v)
          Nothing -> throwE (ParseError s)

evaluate :: [Token] -> Except Err Int
evaluate tokens = do
  let
    stackBinaryOp :: [Int] -> (Int -> Int -> Int) -> Except Err [Int]
    stackBinaryOp (x : y : rest) f = return $ f y x : rest
    stackBinaryOp s _ = throwE (StackSizeError 2 (length s))

    stackUnaryOp :: [Int] -> (Int -> Int) -> Except Err [Int]
    stackUnaryOp (x : rest) f = return $ f x : rest
    stackUnaryOp s _ = throwE (StackSizeError 1 (length s))

    runToken :: [Int] -> Token -> Except Err [Int]
    runToken stack token =
      case token of
        Value x -> return $ x : stack
        Plus -> stackBinaryOp stack (+)
        Minus -> stackBinaryOp stack (-)
        Times -> stackBinaryOp stack (*)
        Divide -> stackBinaryOp stack div
        Negate -> stackUnaryOp stack negate

  stack <- foldM runToken [] tokens
  return (head stack)

calculate :: String -> Either Err Int
calculate input = runExcept $ tokenize input >>= evaluate
