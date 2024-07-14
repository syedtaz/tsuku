{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Manifest where

import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import GHC.Generics
import System.Directory
import System.Environment (lookupEnv)
import System.IO

kTsukuRoot :: String
kTsukuRoot = "TSUKU_ROOT"

kStatefile :: String
kStatefile = "STATE"

data SwitchInfo = SwitchInfo
  { name :: String
  , version :: String
  }
  deriving (Generic, ToJSON, FromJSON)

instance Show SwitchInfo where
  show s = name s ++ " -> " ++ version s

data Manifest = Manifest
  { current :: SwitchInfo
  , installed :: [SwitchInfo]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

readManifest :: IO (Maybe Manifest)
readManifest = do
  root <- getRoot
  let path = root ++ "/" ++ kStatefile
  exists <- doesFileExist path
  readm exists path
 where
  readm False _ = return Nothing
  readm True path = do
    handler <- openFile path ReadMode
    cont <- BL.hGetContents handler
    return (decode cont :: Maybe Manifest)

{- | The current state of the switches is stored in the
root. The location of the root is specified by the
TSUKU_ROOT environment variable if it exists, or the
default location if it doesn't.
-}
getRoot :: IO String
getRoot = do
  loc <- lookupEnv kTsukuRoot
  create loc
 where
  create :: Maybe String -> IO String
  create Nothing = do
    home <- getHomeDirectory
    return (home ++ "/.tsuku")
  create (Just loc) = return loc

rootExists :: IO Bool
rootExists = do
  root <- getRoot
  doesPathExist root

-- | Initializes the tsuku root with a statefile if it doesn't exist already.
initRoot :: IO ()
initRoot = do
  root <- getRoot
  exists <- doesPathExist root
  create exists root
 where
  create :: Bool -> String -> IO ()
  create True _ = return ()
  create False path = do
    createDirectory path
    writeFile (path ++ "/" ++ kStatefile) "{}"

-- Add stuff for initializing an empty statefile
