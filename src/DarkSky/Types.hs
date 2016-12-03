module DarkSky.Types where

import Data.Scientific (Scientific)

data Coordinate = Coordinate
  { latitude :: Degrees
  , longitude :: Degrees
  } deriving (Eq, Show)

type Degrees = Scientific

type Temperature = Scientific

type Distance = Scientific

type PrecipitationAmount = Scientific
