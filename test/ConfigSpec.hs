{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ConfigSpec
  ( configSpec
  ) where

import DarkSky.App.Config
import DarkSky.Types (Coordinate(..), Degrees)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BL
import Data.Monoid ((<>))
import Data.Scientific (fromFloatDigits)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Text.RawString.QQ

instance EqProp Coordinate where
  (=-=) = eq

instance EqProp Config where
  (=-=) = eq

configSpec :: IO ()
configSpec = do
  hspec $
    do describe "Config" $
         describe "FromJSON" $
         do it "full" $
              decode sampleFullConfigJSON `shouldBe` Just sampleFullConfig
            it "empty" $
              decode sampleEmptyConfigJSON `shouldBe` Just (mempty :: Config)
       describe "Monoid" $
         do it "mappends full onto empty" $
              mempty <> sampleFullConfig `shouldBe` sampleFullConfig
            it "mappends empty onto full" $
              sampleFullConfig <> mempty `shouldBe` sampleFullConfig
            it "mappends full onto full" $
              Config (Just "key456") (Just (Coordinate 3 4)) <> sampleFullConfig `shouldBe`
              sampleFullConfig
  quickBatch $ monoid (undefined :: Config)

instance Arbitrary Config where
  arbitrary = Config <$> arbitrary <*> arbitrary

instance Arbitrary Coordinate where
  arbitrary = Coordinate <$> arbitraryDegrees <*> arbitraryDegrees

arbitraryDegrees :: Gen Degrees
arbitraryDegrees = fromFloatDigits <$> (arbitrary :: Gen Double)

sampleFullConfigJSON :: BL.ByteString
sampleFullConfigJSON =
  [r|{
  "key": "key123",
  "latitude": 1,
  "longitude": 2
}|]

sampleFullConfig :: Config
sampleFullConfig =
  Config
  { key = Just "key123"
  , coordinate = Just (Coordinate 1 2)
  }

sampleEmptyConfigJSON :: BL.ByteString
sampleEmptyConfigJSON = "{}"
