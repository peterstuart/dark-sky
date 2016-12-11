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
  DP.emptyDataPoint
  { DP.time = time
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
