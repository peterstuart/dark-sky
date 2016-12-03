module DarkSky.Request where

import DarkSky.Types
import DarkSky.Units
import Data.Time.Clock.POSIX (POSIXTime)

data Request = Request
  { key :: String
  , latitude :: Degrees
  , longitude :: Degrees
  , time :: Maybe POSIXTime
  , excludeBlocks :: [BlockTypes]
  , extendHourly :: Bool
  , language :: Maybe String
  , units :: Maybe Unit
  } deriving (Eq, Show)

data BlockTypes
  = Currently
  | Minutely
  | Hourly
  | Daily
  | Alerts
  | Flags
  deriving (Eq, Show)
