module DarkSky.Response where

import DarkSky.Response.Alerts
import DarkSky.Response.DataBlock
import DarkSky.Response.DataPoint
import DarkSky.Response.Flags
import DarkSky.Types
import Data.Text (Text)

data Response = Response
  { latitude :: Degrees
  , longitude :: Degrees
  , timezone :: Text
  , currently :: DataPoint
  , minutely :: DataBlock
  , hourly :: DataBlock
  , daily :: DataBlock
  , alerts :: [Alert]
  , flags :: Flags
  } deriving (Eq, Show)
