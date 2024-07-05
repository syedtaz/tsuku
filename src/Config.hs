module Config (readConfig) where

import System.Directory (doesFileExist)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

readConfig :: () -> IO (Maybe String)
readConfig () =
  do
    exists <- doesFileExist "Tsukufile"
    contents exists
  where
    contents :: Bool -> IO (Maybe String)
    contents False = return Nothing
    contents True = do
      handler <- openFile "Tsukufile" ReadMode
      cont <- hGetContents handler
      return (Just cont)