{-# LANGUAGE OverloadedStrings #-}

module RequestSpec
  ( requestSpec
  ) where

import DarkSky.Request
import DarkSky.Request.BlockType
import DarkSky.Unit
import DarkSky.Types
import Data.Set as Set
import Data.Time.Clock.POSIX (POSIXTime)
import Test.Hspec

requestSpec :: IO ()
requestSpec =
  hspec $
  describe "Request" $
  do describe "generates a path" $
       do it "with a time" $
            path (sampleRequestEmpty (Just 1)) `shouldBe`
            "/forecast/key/1.1,2.2,1.0"
          it "with no time" $
            path (sampleRequestEmpty Nothing) `shouldBe` "/forecast/key/1.1,2.2"
     describe "generates parameters" $
       do it "full" $
            parameters sampleRequestFull `shouldBe`
            [ ("exclude", "currently,minutely")
            , ("extend", "hourly")
            , ("language", "en")
            , ("units", "uk2")
            ]
          it "empty" $ parameters (sampleRequestEmpty Nothing) `shouldBe` []

sampleRequestEmpty :: Maybe POSIXTime -> Request
sampleRequestEmpty t =
  Request
  { key = "key"
  , coordinate = Coordinate 1.1 2.2
  , time = t
  , excludeBlocks = Set.empty
  , extendHourly = False
  , language = Nothing
  , units = Nothing
  }

sampleRequestFull :: Request
sampleRequestFull =
  Request
  { key = "key"
  , coordinate = Coordinate 1.1 2.2
  , time = Just 1
  , excludeBlocks = fromList [Currently, Minutely]
  , extendHourly = True
  , language = Just "en"
  , units = Just UnitedKingdom
  }
