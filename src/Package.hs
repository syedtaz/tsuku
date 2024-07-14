{-# LANGUAGE OverloadedStrings #-}

module Package where

import Data.Text (Text)
import Toml (TomlCodec, (.=))
import Toml qualified

newtype Config = Config
  { package :: Package
  }

data Package = Package
  { name :: Text
  , version :: Text
  , url :: Text
  , maintainers :: [Text]
  , authors :: [Text]
  , synopsis :: Text
  , tags :: [Text]
  }

configCodec :: TomlCodec Config
configCodec =
  Config <$> Toml.table packageCodec "package" .= package

packageCodec :: TomlCodec Package
packageCodec =
  Package
    <$> Toml.text "name"
    .= name
    <*> Toml.text "version"
    .= version
    <*> Toml.text "url"
    .= url
    <*> Toml.arrayOf Toml._Text "maintainers"
    .= maintainers
    <*> Toml.arrayOf Toml._Text "authors"
    .= authors
    <*> Toml.text "synopsis"
    .= synopsis
    <*> Toml.arrayOf Toml._Text "tags"
    .= tags

read :: FilePath -> IO (Either [Toml.TomlDecodeError] Config)
read file = do
  Toml.decodeFileEither configCodec file
