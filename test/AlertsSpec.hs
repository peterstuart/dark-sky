{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AlertsSpec
  ( alertsSpec
  ) where

import DarkSky.Response.Alerts
import Data.Aeson
import Data.ByteString.Lazy as BL
import Test.Hspec
import Text.RawString.QQ

alertsSpec :: IO ()
alertsSpec =
  hspec $
  describe "Alert" $
  describe "FromJSON" $
  do it "can parse valid JSON" $
       decode sampleAlertJSON `shouldBe` Just sampleAlert
     it "doesn't parse invalid JSON" $
       decode invalidSampleAlertJSON `shouldBe` (Nothing :: Maybe Alert)

sampleAlert :: Alert
sampleAlert =
  Alert
  { description = "A description"
  , expires = 1234
  , title = "A title"
  , uri = "A uri"
  }

sampleAlertJSON :: BL.ByteString
sampleAlertJSON =
  [r|{
  "description": "A description",
  "expires": 1234,
  "title": "A title",
  "uri": "A uri"
}|]

invalidSampleAlertJSON :: BL.ByteString
invalidSampleAlertJSON = [r|{}|]
