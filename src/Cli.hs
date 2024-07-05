{-# LANGUAGE OverloadedStrings #-}

module Cli where

import Control.Lens
import Data.Aeson.Lens
import Data.Text
import Network.Wreq

data Command
  = -- Idea borrowed from opam. A switch is essentially a separate installation
    -- of a compiler alongside a set of packages. You can have multiple versions
    -- of the same compiler with different package constraints.
    Switch

data Switch
  = -- Show the name and compiler version of the current switch.
    Show
  | -- Create a new switch with a specified compiler version or commit hash.
    Create
  | -- Changes the current switch.
    Set
  | -- List currently installed switches.
    List
  | -- List available compiler versions.
    ListAvailable

githubApiUrl :: String
githubApiUrl = "https://api.github.com/repos/koka-lang/koka"

getAvailable :: IO [Text]
getAvailable =
  let opts = defaults & header "X-GitHub-Api-Version" .~ ["2022-11-28"]
   in do
        r <- getWith opts (githubApiUrl ++ "/releases")
        return (r ^. responseBody ^.. values . key "tag_name" . _String)

listAvailable :: IO ()
listAvailable =
  do
    versions <- getAvailable
    putStr (unpack (intercalate (pack "\n") versions))