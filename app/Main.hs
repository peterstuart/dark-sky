{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified DarkSky.App.Config as Config
import DarkSky.App.Config.ArgumentParser (argumentParser)
import DarkSky.Client
import DarkSky.Request
import DarkSky.Response (Response, currently)
import DarkSky.Response.DataPoint (summary, temperature)
import DarkSky.Types
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Set (empty)
import Data.Text (unpack)
import Options.Applicative (execParser)

main :: IO ()
main = do
  config <- execParser argumentParser
  (key', coordinate') <- failEither $ validateConfig config
  let request = makeRequest key' coordinate'
  forecast <- getForecast request
  putStrLn $ output forecast

validateConfig :: Config.Config -> Either String (String, Coordinate)
validateConfig (Config.Config Nothing _) = Left "A key is required."
validateConfig (Config.Config _ Nothing) =
  Left "Latitude and longitude are required."
validateConfig (Config.Config (Just key') (Just coordinate')) =
  Right (key', coordinate')

failEither :: Either String a -> IO a
failEither (Left s) = fail s
failEither (Right a) = return a

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
