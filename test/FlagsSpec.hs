{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module FlagsSpec
  ( flagsSpec
  ) where

import DarkSky.Response.Flags
import DarkSky.Unit
import Data.Aeson
import Data.ByteString.Lazy as BL
import Test.Hspec
import Text.RawString.QQ

flagsSpec :: IO ()
flagsSpec =
  hspec $
  describe "Flags" $
  describe "FromJSON" $
  do describe "it can parse valid JSON" $
       do it "full" $ decode sampleFlagsFullJSON `shouldBe` Just sampleFlagsFull
          it "empty" $
            decode sampleFlagsEmptyJSON `shouldBe` Just sampleFlagsEmpty
     it "doesn't parse invalid JSON" $
       decode invalidSampleFlagsJSON `shouldBe` (Nothing :: Maybe Flags)

sampleFlagsFullJSON :: BL.ByteString
sampleFlagsFullJSON =
  [r|{
  "darksky-unavailable": "unavailable",
  "metno-license": "license",
  "sources": ["source1", "source2"],
  "units": "us"
}|]

sampleFlagsFull :: Flags
sampleFlagsFull =
  Flags
  { darkSkyUnavailable = Just "unavailable"
  , metnoLicense = Just "license"
  , sources = ["source1", "source2"]
  , units = UnitedStates
  }

sampleFlagsEmptyJSON :: BL.ByteString
sampleFlagsEmptyJSON =
  [r|{
  "units": "us",
  "sources": []
}|]

sampleFlagsEmpty :: Flags
sampleFlagsEmpty =
  Flags
  { darkSkyUnavailable = Nothing
  , metnoLicense = Nothing
  , sources = []
  , units = UnitedStates
  }

invalidSampleFlagsJSON :: BL.ByteString
invalidSampleFlagsJSON = "{}"
