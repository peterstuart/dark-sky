{-# LANGUAGE DeriveGeneric #-}

module DarkSky.Response.Alerts where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics

data Alert = Alert
  { description :: Text
  , expires :: POSIXTime
  , title :: Text
  , uri :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Alert
