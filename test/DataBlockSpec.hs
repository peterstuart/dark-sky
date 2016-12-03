{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DataBlockSpec
  ( dataBlockSpec
  ) where

import DarkSky.Response.DataBlock
import qualified DarkSky.Response.DataPoint as DP
import DarkSky.Response.Icons as Icons
import DarkSky.Response.PrecipitationType as PrecipitationType
import Data.Aeson
import Data.ByteString.Lazy as BL
import Test.Hspec
import Text.RawString.QQ

dataBlockSpec :: IO ()
dataBlockSpec =
  hspec $
  describe "DataBlock" $
  describe "FromJSON" $
  do describe "it can parse valid JSON" $
       do it "full" $
            decode sampleFullDataBlockJSON `shouldBe` Just sampleFullDataBlock
          it "empty" $
            decode sampleEmptyDataBlockJSON `shouldBe` Just sampleEmptyDataBlock
     it "doesn't parse invalid JSON" $
       decode invalidSampleDataBlockJSON `shouldBe` (Nothing :: Maybe DataBlock)

sampleFullDataBlockJSON :: BL.ByteString
sampleFullDataBlockJSON =
  [r|{
  "summary": "summary",
  "icon": "rain",
  "data": [{
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
  }]
}|]

sampleFullDataBlock :: DataBlock
sampleFullDataBlock =
  DataBlock
  { data' = [sampleFullDataPoint]
  , summary = Just "summary"
  , icon = Just Icons.Rain
  }

sampleFullDataPoint :: DP.DataPoint
sampleFullDataPoint =
  DP.DataPoint
  { DP.apparentTemperature = Just 1
  , DP.apparentTemperatureMax = Just 2
  , DP.apparentTemperatureMaxTime = Just 3
  , DP.apparentTemperatureMin = Just 4
  , DP.apparentTemperatureMinTime = Just 5
  , DP.cloudCover = Just 0.1
  , DP.dewPoint = Just 6
  , DP.humidity = Just 0.2
  , DP.icon = Just Icons.ClearDay
  , DP.moonPhase = Just 0.3
  , DP.nearestStormBearing = Just 7
  , DP.nearestStormDistance = Just 8
  , DP.ozone = Just 9
  , DP.precipAccumulation = Just 10
  , DP.precipIntensity = Just 11
  , DP.precipIntensityMax = Just 12
  , DP.precipIntensityMaxTime = Just 13
  , DP.precipProbability = Just 0.4
  , DP.precipType = Just PrecipitationType.Rain
  , DP.pressure = Just 14
  , DP.summary = Just "summary"
  , DP.sunriseTime = Just 15
  , DP.sunsetTime = Just 16
  , DP.temperature = Just 17
  , DP.temperatureMax = Just 18
  , DP.temperatureMaxTime = Just 19
  , DP.temperatureMin = Just 20
  , DP.temperatureMinTime = Just 21
  , DP.time = 22
  , DP.visibility = Just 23
  , DP.windBearing = Just 24
  , DP.windSpeed = Just 25
  }

sampleEmptyDataBlockJSON :: BL.ByteString
sampleEmptyDataBlockJSON =
  [r|{
  "data": []
}|]

sampleEmptyDataBlock :: DataBlock
sampleEmptyDataBlock =
  DataBlock
  { data' = []
  , summary = Nothing
  , icon = Nothing
  }

invalidSampleDataBlockJSON :: BL.ByteString
invalidSampleDataBlockJSON = "{}"
