{-# LANGUAGE OverloadedStrings #-}

module DarkSky.Response.PrecipitationType where

import Data.Aeson
import Data.Map as Map
import Data.Maybe (fromMaybe)

data PrecipitationType
  = Rain
  | Snow
  | Sleet
  deriving (Eq, Show)

instance FromJSON PrecipitationType where
  parseJSON = withText "PrecipitationType" fromText
    where
      strings = Map.fromList [("rain", Rain), ("snow", Snow), ("sleet", Sleet)]
      fromText text =
        fromMaybe
          (fail "unrecognized precipitation type")
          (return <$> Map.lookup text strings)
