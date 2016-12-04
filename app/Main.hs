{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import DarkSky.Client
import DarkSky.Request
import DarkSky.Response (Response, currently)
import DarkSky.Response.DataPoint (summary, temperature)
import DarkSky.Types
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Set (empty)
import Data.Text (unpack)
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  (key', coords) <- failMaybe "Invalid arguments" $ parseArgs args
  let request = makeRequest key' coords
  forecast <- getForecast request
  putStrLn $ output forecast

failMaybe :: String -> Maybe a -> IO a
failMaybe _ (Just a) = return a
failMaybe s Nothing = fail s

parseArgs :: [String] -> Maybe (String, Coordinate)
parseArgs [key', lat, lng] = (key', ) <$> coord
  where
    coord = Coordinate <$> readMaybe lat <*> readMaybe lng
parseArgs _ = Nothing

makeRequest :: String -> Coordinate -> Request
makeRequest key' coord =
  Request
  { key = key'
  , coordinate = coord
  , time = Nothing
  , excludeBlocks = empty
  , extendHourly = False
  , language = Nothing
  , units = Nothing
  }

output :: Response -> String
output response = intercalate "\n" [temperatureString, summaryString]
  where
    currentDataPoint = currently response
    temperatureString =
      fromMaybe "No temperature available" $
      ((++ "°") . show) <$> (currentDataPoint >>= temperature)
    summaryString =
      fromMaybe "No summary available" $
      unpack <$> (currentDataPoint >>= summary)
