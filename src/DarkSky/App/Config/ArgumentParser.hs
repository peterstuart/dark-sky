module DarkSky.App.Config.ArgumentParser
  ( argumentParser
  ) where

import DarkSky.App.Config
import DarkSky.Types (Coordinate(..), Degrees)
import Options.Applicative

argumentParser :: ParserInfo Config
argumentParser =
  info
    (helper <*> configParser)
    (fullDesc <>
     progDesc
       "Gets the current weather conditions for the provided coordinates using the Dark Sky API. (https://darksky.net/dev/)")

configParser :: Parser Config
configParser = makeConfig <$> keyOption <*> latOption <*> longOption
  where
    keyOption =
      optional $
      strOption (long "key" <> metavar "KEY" <> help "Dark Sky API key")
    latOption =
      optional $
      option auto (long "lat" <> metavar "LATITUDE" <> help "Latitude")
    longOption =
      optional $
      option auto (long "long" <> metavar "LONGITUDE" <> help "Longitude")

makeConfig :: Maybe String -> Maybe Degrees -> Maybe Degrees -> Config
makeConfig key' latitude' longitude' = Config key' coord
  where
    coord = Coordinate <$> latitude' <*> longitude'
