{-# LANGUAGE OverloadedStrings #-}

module DarkSky.Unit where

import Data.Aeson
import Data.Map as Map
import Data.Maybe (fromMaybe)

data Unit
  = Auto
  | Canada
  | UnitedKingdom
  | UnitedStates
  | SI
  deriving (Eq, Show)

instance FromJSON Unit where
  parseJSON = withText "Unit" fromText
    where
      strings =
        Map.fromList
          [ ("auto", Auto)
          , ("ca", Canada)
          , ("uk2", UnitedKingdom)
          , ("us", UnitedStates)
          , ("si", SI)
          ]
      fromText text =
        fromMaybe (fail "unknown unit") $ return <$> Map.lookup text strings

fromUnit :: Unit -> String
fromUnit Auto = "auto"
fromUnit Canada = "ca"
fromUnit UnitedKingdom = "uk2"
fromUnit UnitedStates = "us"
fromUnit SI = "si"
