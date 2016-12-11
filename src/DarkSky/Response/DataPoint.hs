{-# LANGUAGE DeriveGeneric #-}

module DarkSky.Response.DataPoint where

import DarkSky.Response.PrecipitationType
import DarkSky.Response.Icon
import DarkSky.Types
import Data.Aeson
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics

data DataPoint = DataPoint
  { apparentTemperature :: Maybe Temperature
  , apparentTemperatureMax :: Maybe Temperature
  , apparentTemperatureMaxTime :: Maybe POSIXTime
  , apparentTemperatureMin :: Maybe Temperature
  , apparentTemperatureMinTime :: Maybe POSIXTime
  , cloudCover :: Maybe Scientific
  , dewPoint :: Maybe Temperature
  , humidity :: Maybe Scientific
  , icon :: Maybe Icon
  , moonPhase :: Maybe Scientific
  , nearestStormBearing :: Maybe Scientific
  , nearestStormDistance :: Maybe Distance
  , ozone :: Maybe Scientific
  , precipAccumulation :: Maybe PrecipitationAmount
  , precipIntensity :: Maybe PrecipitationAmount
  , precipIntensityMax :: Maybe PrecipitationAmount
  , precipIntensityMaxTime :: Maybe POSIXTime
  , precipProbability :: Maybe Scientific
  , precipType :: Maybe PrecipitationType
  , pressure :: Maybe Scientific
  , summary :: Maybe Text
  , sunriseTime :: Maybe POSIXTime
  , sunsetTime :: Maybe POSIXTime
  , temperature :: Maybe Temperature
  , temperatureMax :: Maybe Temperature
  , temperatureMaxTime :: Maybe POSIXTime
  , temperatureMin :: Maybe Temperature
  , temperatureMinTime :: Maybe POSIXTime
  , time :: POSIXTime
  , visibility :: Maybe Distance
  , windBearing :: Maybe Scientific
  , windSpeed :: Maybe Scientific
  } deriving (Eq, Show, Generic)

instance FromJSON DataPoint

emptyDataPoint :: DataPoint
emptyDataPoint =
  DataPoint
  { apparentTemperature = Nothing
  , apparentTemperatureMax = Nothing
  , apparentTemperatureMaxTime = Nothing
  , apparentTemperatureMin = Nothing
  , apparentTemperatureMinTime = Nothing
  , cloudCover = Nothing
  , dewPoint = Nothing
  , humidity = Nothing
  , icon = Nothing
  , moonPhase = Nothing
  , nearestStormBearing = Nothing
  , nearestStormDistance = Nothing
  , ozone = Nothing
  , precipAccumulation = Nothing
  , precipIntensity = Nothing
  , precipIntensityMax = Nothing
  , precipIntensityMaxTime = Nothing
  , precipProbability = Nothing
  , precipType = Nothing
  , pressure = Nothing
  , summary = Nothing
  , sunriseTime = Nothing
  , sunsetTime = Nothing
  , temperature = Nothing
  , temperatureMax = Nothing
  , temperatureMaxTime = Nothing
  , temperatureMin = Nothing
  , temperatureMinTime = Nothing
  , time = 0
  , visibility = Nothing
  , windBearing = Nothing
  , windSpeed = Nothing
  }
