module Main where

  import Switch ( getReleases )
  
  main :: IO ()
  main = do
    xs <- getReleases
    print xs
