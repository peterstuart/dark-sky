{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module DarkSky.Request where

import DarkSky.Request.BlockType
import DarkSky.Types
import DarkSky.Unit
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Set (Set, toList)
import Data.Scientific (Scientific)
import Data.Text (Text, unpack)
import Data.Time.Clock.POSIX (POSIXTime)

data Request = Request
  { key :: String
  , coordinate :: Coordinate
  , time :: Maybe POSIXTime
  , excludeBlocks :: Set BlockType
  , extendHourly :: Bool
  , language :: Maybe Text
  , units :: Maybe Unit
  } deriving (Eq, Show)

path :: Request -> String
path request = "/forecast/" ++ key request ++ "/" ++ components
  where
    coordinate' = coordinate request
    latitude' = show $ latitude coordinate'
    longitude' = show $ longitude coordinate'
    stime = (realToFrac <$> time request) :: Maybe Scientific
    time' = show <$> stime
    components =
      intercalate "," $ catMaybes [Just latitude', Just longitude', time']

parameters :: Request -> [(String, String)]
parameters request =
  catMaybes
    [ if null (excludeBlocks request)
        then Nothing
        else Just
               ( "exclude"
               , intercalate "," $
                 fromBlockType <$> toList (excludeBlocks request))
    , if extendHourly request
        then Just ("extend", "hourly")
        else Nothing
    , ("language", ) . unpack <$> language request
    , ("units", ) . fromUnit <$> units request
    ]
