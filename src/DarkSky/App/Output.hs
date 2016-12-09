module DarkSky.App.Output where

import DarkSky.Response
import qualified DarkSky.Response.DataBlock as DB
import qualified DarkSky.Response.DataPoint as DP
import Data.Maybe (fromMaybe)
import Data.Text (unpack)

data Output = Output
  { currentTemperature :: String
  , currentSummary :: String
  , next48HoursSummary :: String
  , weekSummary :: String
  } deriving (Eq, Show)

makeOutput :: Response -> Output
makeOutput response =
  Output
  { currentTemperature = currentTemperatureString
  , currentSummary = currentSummaryString
  , next48HoursSummary = next48HoursSummaryString
  , weekSummary = weekSummaryString
  }
  where
    currentDP = currently response
    currentTemperatureString =
      fromMaybe "No temperature available" $
      ((++ "°") . show) <$> (currentDP >>= DP.temperature)
    currentSummaryString =
      fromMaybe "No summary available" $ unpack <$> (currentDP >>= DP.summary)
    next48HoursDB = hourly response
    next48HoursSummaryString =
      fromMaybe "No summary available" $
      unpack <$> (next48HoursDB >>= DB.summary)
    weekDB = daily response
    weekSummaryString =
      fromMaybe "No summary available" $ unpack <$> (weekDB >>= DB.summary)
