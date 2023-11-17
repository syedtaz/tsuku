{-# LANGUAGE OverloadedStrings #-}

module Switch where

  import Network.Wreq ( getWith, defaults, header, responseBody, responseStatus)
  import Control.Lens ( (&), (^.), (.~), (^..))
  import Data.Aeson.Lens (key, _String, values)
  import Data.Text ( Text )
  import qualified Data.ByteString as BS
  import Network.HTTP.Types ( Status(Status, statusCode) )
  
  githubApiVersion :: BS.ByteString
  githubApiVersion = "2022-11-28"

  kokaReleasePrefix :: String
  kokaReleasePrefix = "https://api.github.com/repos/koka-lang/koka/releases"
  
  getReleases :: IO (Maybe[Text])
  getReleases = 
    do 
      resp <- getWith opts kokaReleasePrefix
      case resp ^. responseStatus of
        Status { statusCode = 200 } -> return $ Just $ resp ^. responseBody ^.. values . key "tag_name" . _String
        Status _ _ -> return Nothing
    where opts = defaults & header "Accept" .~ ["application/vnd.github+json"] & header "X-GitHub-Api-Version" .~ [githubApiVersion]
      
 