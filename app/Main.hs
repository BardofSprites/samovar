module Main where

import Control.Monad (unless)
import System.IO
import Data.Char

read' :: IO String
read' = putStr "🥭> "
      >> hFlush stdout
      >> getLine

print' :: String -> IO ()
print' = putStrLn

-- Function to capitalize each word in the input string
capitalizer :: String -> Maybe String
capitalizer input
  | null input = Nothing
  | otherwise  = Just $ unwords $ map capitalize $ words input
  where capitalize (h:t) = toUpper h : map toLower t
        capitalize _     = []

-- Function to echo input string with a prefix
doEcho :: String -> Maybe String
doEcho input
  | null input = Nothing
  | otherwise  = Just $ "echoing: " ++ input

-- Evaluator function to determine which command to execute
evaluatorFunction :: String -> Maybe String
evaluatorFunction input =
  case words input of
       ("echo":rest) -> doEcho (unwords rest)
       ("capitalize":rest) -> capitalizer (unwords rest)
       _             -> Nothing  -- Return Nothing for unknown commands

-- Wrapper function to handle the result of evaluatorFunction
eval' :: String -> String
eval' input =
  case evaluatorFunction input of
    Just result -> result
    Nothing     -> "Error: Unknown command or empty input"

main :: IO ()
main = do
     input <- read'
     unless (input == ":q") $ print' (eval' input) >> main
