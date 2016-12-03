{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PrecipitationTypeSpec
  ( precipitationTypeSpec
  ) where

import DarkSky.Response.PrecipitationType
import Data.Aeson
import Data.ByteString.Lazy as BL
import Test.Hspec
import Text.RawString.QQ

precipitationTypeSpec :: IO ()
precipitationTypeSpec =
  hspec $
  describe "PrecipitationType" $
  describe "FromJSON" $
  do it "can parse valid JSON" $
       decode samplePrecipitationTypes `shouldBe` Just [Rain, Snow, Sleet]
     it "doesn't parse invalid JSON" $
       decode sampleInvalidData `shouldBe`
       (Nothing :: Maybe [PrecipitationType])

samplePrecipitationTypes :: BL.ByteString
samplePrecipitationTypes = [r|["rain", "snow", "sleet"]|]

sampleInvalidData :: BL.ByteString
sampleInvalidData = [r|["gibberish"]|]
