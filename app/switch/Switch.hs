{-# LANGUAGE OverloadedStrings #-}

module Switch (getRelease, getReleases) where

  import qualified Network.Wreq as Wreq
  import Control.Lens ( (&), (^.), (.~), (^..))
  import qualified Data.Aeson.Lens as Aeson.Lens
  import qualified Network.HTTP.Types as HTTP.Types
  import qualified Data.Text as T
  
  defaultOpts :: Wreq.Options
  defaultOpts = Wreq.defaults & acceptHeader & versionHeader
    where acceptHeader  = Wreq.header "Accept" .~ ["application/vnd.github+json"]
          versionHeader = Wreq.header "X-GitHub-Api-Version" .~ [apiVersion]
          apiVersion    = "2022-11-28"

  -- Functions for listing all releases.

  mapTagVers :: Aeson.Lens.AsValue s => Wreq.Response s -> [(T.Text, T.Text)]
  mapTagVers resp = zip tags vers
    where tags    = view $ Aeson.Lens.key "tag_name"
          vers    = view $ Aeson.Lens.key "published_at"
          view f  = resp ^. Wreq.responseBody ^.. Aeson.Lens.values . f . Aeson.Lens._String 
    
  getReleases :: IO (Maybe[(T.Text, T.Text)])
  getReleases = do
    resp <- Wreq.getWith defaultOpts "https://api.github.com/repos/koka-lang/koka/releases"
    case resp ^. Wreq.responseStatus of
      HTTP.Types.Status { HTTP.Types.statusCode = 200 } -> return $ Just $ mapTagVers resp
      HTTP.Types.Status _ _ -> return Nothing

  -- Functions getting a specific release.

  -- TODO! Fix
  getRelease :: T.Text -> IO (Maybe [T.Text])
  getRelease tag = do
      resp <- Wreq.getWith defaultOpts apiUrl
      case resp ^. Wreq.responseStatus of
        -- HTTP.Types.Status { HTTP.Types.statusCode = 200 } -> return $ Just $ extractTag resp
        HTTP.Types.Status _ _ -> return Nothing
    where
      apiUrl = T.unpack $ "https://api.github.com/repos/koka-lang/koka/releases/tags/" <> tag