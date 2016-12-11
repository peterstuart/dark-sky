{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DataPointSpec
  ( dataPointSpec
  ) where

import DarkSky.Response.DataPoint
import DarkSky.Response.Icon as Icon
import DarkSky.Response.PrecipitationType as PrecipitationType
import Data.Aeson
import Data.ByteString.Lazy as BL
import Test.Hspec
import Text.RawString.QQ

dataPointSpec :: IO ()
dataPointSpec =
  hspec $
  describe "DataPoint" $
  describe "FromJSON" $
  do describe "it can parse valid JSON" $
       do it "full" $
            decode sampleFullDataPointJSON `shouldBe` Just sampleFullDataPoint
          it "empty" $
            decode sampleEmptyDataPointJSON `shouldBe` Just sampleEmptyDataPoint
     it "doesn't parse invalid JSON" $
       decode invalidSampleDataPointJSON `shouldBe` (Nothing :: Maybe DataPoint)

sampleFullDataPointJSON :: BL.ByteString
sampleFullDataPointJSON =
  [r|{
  "apparentTemperature": 1,
  "apparentTemperatureMax": 2,
  "apparentTemperatureMaxTime": 3,
  "apparentTemperatureMin": 4,
  "apparentTemperatureMinTime": 5,
  "cloudCover": 0.1,
  "dewPoint": 6,
  "humidity": 0.2,
  "icon": "clear-day",
  "moonPhase": 0.3,
  "nearestStormBearing": 7,
  "nearestStormDistance": 8,
  "ozone": 9,
  "precipAccumulation": 10,
  "precipIntensity": 11,
  "precipIntensityMax": 12,
  "precipIntensityMaxTime": 13,
  "precipProbability": 0.4,
  "precipType": "rain",
  "pressure": 14,
  "summary": "summary",
  "sunriseTime": 15,
  "sunsetTime": 16,
  "temperature": 17,
  "temperatureMax": 18,
  "temperatureMaxTime": 19,
  "temperatureMin": 20,
  "temperatureMinTime": 21,
  "time": 22,
  "visibility": 23,
  "windBearing": 24,
  "windSpeed": 25
}|]

sampleFullDataPoint :: DataPoint
sampleFullDataPoint =
  DataPoint
  { apparentTemperature = Just 1
  , apparentTemperatureMax = Just 2
  , apparentTemperatureMaxTime = Just 3
  , apparentTemperatureMin = Just 4
  , apparentTemperatureMinTime = Just 5
  , cloudCover = Just 0.1
  , dewPoint = Just 6
  , humidity = Just 0.2
  , icon = Just Icon.ClearDay
  , moonPhase = Just 0.3
  , nearestStormBearing = Just 7
  , nearestStormDistance = Just 8
  , ozone = Just 9
  , precipAccumulation = Just 10
  , precipIntensity = Just 11
  , precipIntensityMax = Just 12
  , precipIntensityMaxTime = Just 13
  , precipProbability = Just 0.4
  , precipType = Just PrecipitationType.Rain
  , pressure = Just 14
  , summary = Just "summary"
  , sunriseTime = Just 15
  , sunsetTime = Just 16
  , temperature = Just 17
  , temperatureMax = Just 18
  , temperatureMaxTime = Just 19
  , temperatureMin = Just 20
  , temperatureMinTime = Just 21
  , time = 22
  , visibility = Just 23
  , windBearing = Just 24
  , windSpeed = Just 25
  }

sampleEmptyDataPointJSON :: BL.ByteString
sampleEmptyDataPointJSON =
  [r|{
  "time": 1
}|]

sampleEmptyDataPoint :: DataPoint
sampleEmptyDataPoint =
  emptyDataPoint
  { time = 1
  }

invalidSampleDataPointJSON :: BL.ByteString
invalidSampleDataPointJSON = "{}"
