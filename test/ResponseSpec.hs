{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ResponseSpec
  ( responseSpec
  ) where

import DarkSky.Response
import DarkSky.Response.Alert
import DarkSky.Response.DataBlock
import qualified DarkSky.Response.DataPoint as DP
import DarkSky.Response.Flags
import DarkSky.Types
import DarkSky.Unit
import Data.Aeson
import Data.ByteString.Lazy as BL
import Data.Time.Clock.POSIX (POSIXTime)
import Test.Hspec
import Text.RawString.QQ

responseSpec :: IO ()
responseSpec =
  hspec $
  describe "Response" $
  describe "FromJSON" $
  do describe "it can parse valid JSON" $
       do it "full" $
            decode sampleFullResponseJSON `shouldBe` Just sampleFullResponse
          it "empty" $
            decode sampleEmptyResponseJSON `shouldBe` Just sampleEmptyResponse
     it "doesn't parse invalid JSON" $
       decode invalidSampleResponseJSON `shouldBe` (Nothing :: Maybe Response)

sampleFullResponseJSON :: BL.ByteString
sampleFullResponseJSON =
  [r|{
  "latitude": 1,
  "longitude": 2,
  "timezone": "America/Los_Angeles",
  "currently": {
    "time": 1
  },
  "minutely" : {
    "data": []
  },
  "hourly" : {
    "data": []
  },
  "daily" : {
    "data": []
  },
  "minutely" : {
    "data": []
  },
  "alerts": [
    {
      "description": "description",
      "expires": 1,
      "title": "title",
      "uri": "http://www.example.com"
    }
  ],
  "flags": {
    "sources": [],
    "units": "us"
  }
}|]

sampleFullResponse :: Response
sampleFullResponse =
  Response
  { coordinate = Coordinate 1 2
  , timezone = "America/Los_Angeles"
  , currently = Just $ emptyDataPoint 1
  , minutely = Just emptyDataBlock
  , hourly = Just emptyDataBlock
  , daily = Just emptyDataBlock
  , alerts = [Alert "description" 1 "title" "http://www.example.com"]
  , flags = Just $ Flags Nothing Nothing [] UnitedStates
  }

emptyDataPoint :: POSIXTime -> DP.DataPoint
emptyDataPoint time =
  DP.DataPoint
  { DP.apparentTemperature = Nothing
  , DP.apparentTemperatureMax = Nothing
  , DP.apparentTemperatureMaxTime = Nothing
  , DP.apparentTemperatureMin = Nothing
  , DP.apparentTemperatureMinTime = Nothing
  , DP.cloudCover = Nothing
  , DP.dewPoint = Nothing
  , DP.humidity = Nothing
  , DP.icon = Nothing
  , DP.moonPhase = Nothing
  , DP.nearestStormBearing = Nothing
  , DP.nearestStormDistance = Nothing
  , DP.ozone = Nothing
  , DP.precipAccumulation = Nothing
  , DP.precipIntensity = Nothing
  , DP.precipIntensityMax = Nothing
  , DP.precipIntensityMaxTime = Nothing
  , DP.precipProbability = Nothing
  , DP.precipType = Nothing
  , DP.pressure = Nothing
  , DP.summary = Nothing
  , DP.sunriseTime = Nothing
  , DP.sunsetTime = Nothing
  , DP.temperature = Nothing
  , DP.temperatureMax = Nothing
  , DP.temperatureMaxTime = Nothing
  , DP.temperatureMin = Nothing
  , DP.temperatureMinTime = Nothing
  , DP.time = time
  , DP.visibility = Nothing
  , DP.windBearing = Nothing
  , DP.windSpeed = Nothing
  }

emptyDataBlock :: DataBlock
emptyDataBlock =
  DataBlock
  { data' = []
  , summary = Nothing
  , icon = Nothing
  }

sampleEmptyResponseJSON :: BL.ByteString
sampleEmptyResponseJSON =
  [r|{
  "latitude": 1,
  "longitude": 2,
  "timezone": "America/Los_Angeles"
}|]

sampleEmptyResponse :: Response
sampleEmptyResponse =
  Response
  { coordinate = Coordinate 1 2
  , timezone = "America/Los_Angeles"
  , currently = Nothing
  , minutely = Nothing
  , hourly = Nothing
  , daily = Nothing
  , alerts = []
  , flags = Nothing
  }

invalidSampleResponseJSON :: BL.ByteString
invalidSampleResponseJSON = "{}"
