{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module UnitSpec
  ( unitSpec
  ) where

import DarkSky.Unit
import Data.Aeson
import Data.ByteString.Lazy as BL
import Test.Hspec
import Text.RawString.QQ

unitSpec :: IO ()
unitSpec =
  hspec $
  describe "Unit" $
  do describe "FromJSON" $
       do it "can parse valid JSON" $
            decode sampleUnitsJSON `shouldBe` Just sampleUnits
          it "doesn't parse invalid JSON" $
            decode invalidUnitsJSON `shouldBe` (Nothing :: Maybe [Unit])
     it "generates a string" $
       fromUnit <$> sampleUnits `shouldBe` ["auto", "ca", "uk2", "us", "si"]

sampleUnitsJSON :: BL.ByteString
sampleUnitsJSON = [r|["auto","ca","uk2","us","si"]|]

sampleUnits :: [Unit]
sampleUnits = [Auto, Canada, UnitedKingdom, UnitedStates, SI]

invalidUnitsJSON :: BL.ByteString
invalidUnitsJSON = [r|["gibberish"]|]
