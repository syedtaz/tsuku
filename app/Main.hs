{-# LANGUAGE OverloadedStrings #-}

module Main where

  import Network.Wreq hiding (statusCode)
  import Control.Lens
  import Network.HTTP.Types
  import Data.Text (Text)
  
  getReleases :: IO ()
  getReleases = 
    let 
      githubApiVersion = "2022-11-28"
      kokaReleasePrefix = "https://api.github.com/repos/koka-lang/koka/releases"
      opts = defaults & header "Accept" .~ ["application/vnd.github+json"] & header "X-GitHub-Api-Version" .~ [githubApiVersion]
    in do 
      r <- getWith opts kokaReleasePrefix
      case r ^. responseStatus of
        Status { statusCode = 200 } -> print success
        Status _ _ -> print failure
      where 
        success :: Text = "Success :)"
        failure :: Text = "Failure :("

      -- print $ r ^. responseBody . key "Assets" . _String

  main :: IO ()
  main = getReleases
