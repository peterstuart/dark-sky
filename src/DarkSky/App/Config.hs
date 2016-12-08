{-# LANGUAGE OverloadedStrings #-}

module DarkSky.App.Config
  ( Config(..)
  ) where

import DarkSky.Types
import Data.Aeson

data Config = Config
  { key :: Maybe String
  , coordinate :: Maybe Coordinate
  } deriving (Eq, Show)

instance Monoid Config where
  mempty =
    Config
    { key = Nothing
    , coordinate = Nothing
    }
  mappend (Config key1 coord1) (Config key2 coord2) =
    Config
    { key = key2 `orElse` key1
    , coordinate = coord2 `orElse` coord1
    }

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just a) _ = Just a
orElse _ x = x

instance FromJSON Config where
  parseJSON =
    withObject "config" $
    \o -> do
      key' <- o .:? "key"
      latitude' <- o .:? "latitude"
      longitude' <- o .:? "longitude"
      return
        Config
        { key = key'
        , coordinate = Coordinate <$> latitude' <*> longitude'
        }
