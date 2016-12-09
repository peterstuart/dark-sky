{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified DarkSky.App.Config as Config
import DarkSky.App.Config.ArgumentParser (argumentParser)
import DarkSky.App.Output
import DarkSky.Client
import DarkSky.Request
import DarkSky.Types
import Data.List (intercalate)
import Data.Set (empty)
import Options.Applicative (execParser)

main :: IO ()
main = do
  config <- execParser argumentParser
  (key', coordinate') <- failEither $ validateConfig config
  let request = makeRequest key' coordinate'
  forecast <- getForecast request
  let o = makeOutput forecast
  putStrLn $ output o

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

output :: Output -> String
output o =
  intercalate
    "\n"
    [ "Now:"
    , currentTemperature o
    , currentSummary o
    , ""
    , "Next 48 Hours:"
    , next48HoursSummary o
    , ""
    , "Week:"
    , weekSummary o
    ]
