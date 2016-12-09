{-# LANGUAGE OverloadedStrings #-}

module OutputSpec
  ( outputSpec
  ) where

import DarkSky.App.Output
import DarkSky.Response
import qualified DarkSky.Response.DataBlock as DB
import qualified DarkSky.Response.DataPoint as DP
import DarkSky.Types
import Data.Text (Text)
import Test.Hspec

outputSpec :: IO ()
outputSpec =
  hspec $
  describe "Output" $
  describe "creating" $
  do it "from a full response" $ makeOutput fullResponse `shouldBe` fullOutput
     it "from an empty response" $
       makeOutput emptyResponse `shouldBe` emptyOutput

fullResponse :: Response
fullResponse =
  Response
  { coordinate = Coordinate 1 2
  , timezone = "timezone"
  , currently = Just $ dataPoint 12.3 "current summary"
  , minutely = Nothing
  , hourly = Just $ dataBlock "next 48 hours summary"
  , daily = Just $ dataBlock "week summary"
  , alerts = []
  , flags = Nothing
  }

fullOutput :: Output
fullOutput =
  Output
  { currentTemperature = "12.3°"
  , currentSummary = "current summary"
  , next48HoursSummary = "next 48 hours summary"
  , weekSummary = "week summary"
  }

emptyResponse :: Response
emptyResponse =
  Response
  { coordinate = Coordinate 1 2
  , timezone = "timezone"
  , currently = Nothing
  , minutely = Nothing
  , hourly = Nothing
  , daily = Nothing
  , alerts = []
  , flags = Nothing
  }

emptyOutput :: Output
emptyOutput =
  Output
  { currentTemperature = "No temperature available"
  , currentSummary = "No summary available"
  , next48HoursSummary = "No summary available"
  , weekSummary = "No summary available"
  }

dataPoint :: Temperature -> Text -> DP.DataPoint
dataPoint temperature' summary' =
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
  , DP.summary = Just summary'
  , DP.sunriseTime = Nothing
  , DP.sunsetTime = Nothing
  , DP.temperature = Just temperature'
  , DP.temperatureMax = Nothing
  , DP.temperatureMaxTime = Nothing
  , DP.temperatureMin = Nothing
  , DP.temperatureMinTime = Nothing
  , DP.time = 1
  , DP.visibility = Nothing
  , DP.windBearing = Nothing
  , DP.windSpeed = Nothing
  }

dataBlock :: Text -> DB.DataBlock
dataBlock summary' =
  DB.DataBlock
  { DB.data' = []
  , DB.summary = Just summary'
  , DB.icon = Nothing
  }
