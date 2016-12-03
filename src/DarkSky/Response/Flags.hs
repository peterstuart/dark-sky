{-# LANGUAGE OverloadedStrings #-}

module DarkSky.Response.Flags where

import Data.Aeson
import Data.Text (Text)

data Flags = Flags
  { darkSkyUnavailable :: Maybe Text
  , metnoLicense :: Maybe Text
  , sources :: [Text]
  , units :: Text
  } deriving (Eq, Show)

instance FromJSON Flags where
  parseJSON =
    withObject "flags" $
    \o -> do
      darkSkyUnavailable' <- o .:? "darksky-unavailable"
      metnoLicense' <- o .:? "metno-license"
      sources' <- o .: "sources"
      units' <- o .: "units"
      return
        Flags
        { darkSkyUnavailable = darkSkyUnavailable'
        , metnoLicense = metnoLicense'
        , sources = sources'
        , units = units'
        }
