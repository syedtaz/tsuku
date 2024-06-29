module Main where

import qualified Config (readConfig)

main :: IO ()
main = do
  res <- Config.readConfig ()
  case res of
    Just v -> print v
    Nothing -> putStrLn "No Tsukufile in current directory."