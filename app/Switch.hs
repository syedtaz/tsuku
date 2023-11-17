{-# LANGUAGE OverloadedStrings #-}

module Switch (getReleases) where

  import qualified Network.Wreq as Wreq
  import Control.Lens ( (&), (^.), (.~), (^..))
  import qualified Data.Aeson.Lens as Aeson.Lens
  import qualified Data.ByteString as ByteString
  import qualified Network.HTTP.Types as HTTP.Types
  import qualified Data.Text as T
  
  githubApiVersion :: ByteString.ByteString
  githubApiVersion = "2022-11-28"

  kokaReleasePrefix :: String
  kokaReleasePrefix = "https://api.github.com/repos/koka-lang/koka/releases"  

  getReleases :: IO (Maybe[(T.Text, T.Text)])
  getReleases =
    do
      resp <- Wreq.getWith opts kokaReleasePrefix
      case resp ^. Wreq.responseStatus of
        HTTP.Types.Status { HTTP.Types.statusCode = 200 } ->
          let view f = resp ^. Wreq.responseBody ^.. Aeson.Lens.values . f . Aeson.Lens._String 
              tags = view $ Aeson.Lens.key "tag_name"
              vers = view $ Aeson.Lens.key "published_at"
          in return $ Just $ zip tags vers
        HTTP.Types.Status _ _ -> return Nothing
    where opts = Wreq.defaults & Wreq.header "Accept" .~ ["application/vnd.github+json"] & Wreq.header "X-GitHub-Api-Version" .~ [githubApiVersion]