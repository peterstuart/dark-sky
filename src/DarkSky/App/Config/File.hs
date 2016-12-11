module DarkSky.App.Config.File
  ( configPath
  , fromFile
  ) where

import Control.Exception (SomeException, try)
import DarkSky.App.Config
import Data.Aeson (decode)
import Data.ByteString.Lazy as BL
import Data.Either.Combinators (rightToMaybe)
import Data.Maybe (fromMaybe)
import System.Directory (getAppUserDataDirectory)

configPath :: IO FilePath
configPath = (++ "/config.json") <$> getAppUserDataDirectory "dark-sky"

fromFile :: IO Config
fromFile = do
  path <- configPath
  jsonData <-
    rightToMaybe <$>
    (try (BL.readFile path) :: IO (Either SomeException BL.ByteString))
  let config = jsonData >>= decode
  return $ fromMaybe mempty config
