{-# LANGUAGE OverloadedStrings #-}

module DarkSky.Response where

import DarkSky.Response.Alert
import DarkSky.Response.DataBlock
import DarkSky.Response.DataPoint
import DarkSky.Response.Flags
import DarkSky.Types
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Text (Text)

data Response = Response
  { coordinate :: Coordinate
  , timezone :: Text
  , currently :: Maybe DataPoint
  , minutely :: Maybe DataBlock
  , hourly :: Maybe DataBlock
  , daily :: Maybe DataBlock
  , alerts :: [Alert]
  , flags :: Maybe Flags
  } deriving (Eq, Show)

instance FromJSON Response where
  parseJSON =
    withObject "response" $
    \o -> do
      latitude' <- o .: "latitude"
      longitude' <- o .: "longitude"
      let coordinate' = Coordinate latitude' longitude'
      timezone' <- o .: "timezone"
      currently' <- o .:? "currently"
      minutely' <- o .:? "minutely"
      hourly' <- o .:? "hourly"
      daily' <- o .:? "daily"
      alerts' <- fromMaybe [] <$> o .:? "alerts"
      flags' <- o .:? "flags"
      return
        Response
        { coordinate = coordinate'
        , timezone = timezone'
        , currently = currently'
        , minutely = minutely'
        , hourly = hourly'
        , daily = daily'
        , alerts = alerts'
        , flags = flags'
        }
