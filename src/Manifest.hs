{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Manifest where

import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import GHC.Generics
import System.Directory
import System.IO

data SwitchInfo = SwitchInfo
  { name :: String,
    version :: String
  }
  deriving (Generic, ToJSON, FromJSON)

instance Show SwitchInfo where
  show s = name s ++ " -> " ++ version s

data Manifest = Manifest
  { current :: SwitchInfo,
    installed :: [SwitchInfo]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

readManifest :: IO (Maybe Manifest)
readManifest =
  do
    home <- getHomeDirectory
    let path = home ++ "/.tsuku/STATE"
    exists <- doesFileExist path
    readm exists path
  where
    readm False _ = return Nothing
    readm True path =
      do
        handler <- openFile path ReadMode
        cont <- BL.hGetContents handler
        return (decode cont :: Maybe Manifest)
