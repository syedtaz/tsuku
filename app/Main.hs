-- {-# LANGUAGE OverloadedStrings #-}

module Main where

  import Switch
  
  main :: IO ()
  main = do
    xs <- getReleases
    print xs
