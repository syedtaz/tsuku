{-# LANGUAGE OverloadedStrings #-}

module Cli where

import Control.Lens
import Data.Aeson.Lens
import Data.Text
import Manifest (Manifest, current, installed, readManifest)
import Network.Wreq

data Command
  = -- Idea borrowed from opam. A switch is essentially a separate installation
    -- of a compiler alongside a set of packages. You can have multiple versions
    -- of the same compiler with different package constraints.
    Switch

data Switch
  = -- Show the name and compiler version of the current switch.
    Which
  | -- Create a new switch with a specified compiler version or commit hash.
    Create
  | -- Changes the current switch.
    Set
  | -- List currently installed switches.
    List
  | -- List available compiler versions.
    ListAvailable

getAvailable :: IO [Text]
getAvailable =
  let opts = defaults & header "X-GitHub-Api-Version" .~ [githubApiVersion]
   in do
        r <- getWith opts (githubApiUrl ++ "/releases")
        return (r ^. responseBody ^.. values . key "tag_name" . _String)
  where
    githubApiVersion = "2022-11-28"
    githubApiUrl = "https://api.github.com/repos/koka-lang/koka"

listAvailable :: IO ()
listAvailable =
  do
    versions <- getAvailable
    putStr (unpack (intercalate (pack "\n") versions))

which :: IO ()
which =
  do
    state <- readManifest
    print (which' state)
  where
    which' :: Maybe Manifest -> String
    which' Nothing = "No switch currently installed."
    which' (Just manifest) = show $ current manifest

list :: IO ()
list =
  do
    state <- readManifest
    putStr (getList state)
    putStr "\n"
  where
    getList :: Maybe Manifest -> String
    getList Nothing = "No installed versions found."
    getList (Just manifest) =
      let lst = fmap (pack . show) (current manifest : installed manifest)
       in unpack $ intercalate (pack "\n") lst