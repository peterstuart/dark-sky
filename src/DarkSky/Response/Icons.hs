{-# LANGUAGE OverloadedStrings #-}

module DarkSky.Response.Icons where

import Data.Aeson
import Data.Map as Map
import Data.Maybe (fromMaybe)

data Icon
  = ClearDay
  | ClearNight
  | Rain
  | Snow
  | Sleet
  | Wind
  | Fog
  | Cloudy
  | PartlyCloudyDay
  | PartlyCloudyNight
  | Unknown
  deriving (Eq, Show)

instance FromJSON Icon where
  parseJSON = withText "Alert" fromText
    where
      strings =
        Map.fromList
          [ ("clear-day", ClearDay)
          , ("clear-night", ClearNight)
          , ("rain", Rain)
          , ("snow", Snow)
          , ("sleet", Sleet)
          , ("wind", Wind)
          , ("fog", Fog)
          , ("cloudy", Cloudy)
          , ("partly-cloudy-day", PartlyCloudyDay)
          , ("partly-cloudy-night", PartlyCloudyNight)
          ]
      fromText text = return $ fromMaybe Unknown $ Map.lookup text strings
