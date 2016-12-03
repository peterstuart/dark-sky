{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module IconSpec
  ( iconSpec
  ) where

import DarkSky.Response.Icon
import Data.Aeson
import Data.ByteString.Lazy as BL
import Test.Hspec
import Text.RawString.QQ

iconSpec :: IO ()
iconSpec =
  hspec $
  describe "Icon" $
  describe "FromJSON" $
  it "can parse valid JSON" $
  decode sampleIcons `shouldBe`
  Just
    [ ClearDay
    , ClearNight
    , Rain
    , Snow
    , Sleet
    , Wind
    , Fog
    , Cloudy
    , PartlyCloudyDay
    , PartlyCloudyNight
    , Unknown
    ]

sampleIcons :: BL.ByteString
sampleIcons =
  [r|["clear-day", "clear-night", "rain", "snow", "sleet", "wind", "fog", "cloudy", "partly-cloudy-day", "partly-cloudy-night", "gibberish"]|]
