{-# LANGUAGE OverloadedStrings #-}

module DarkSky.Response.DataBlock where

import DarkSky.Response.Icon
import DarkSky.Response.DataPoint (DataPoint)
import Data.Aeson
import Data.Text (Text)

data DataBlock = DataBlock
  { data' :: [DataPoint]
  , summary :: Maybe Text
  , icon :: Maybe Icon
  } deriving (Eq, Show)

instance FromJSON DataBlock where
  parseJSON =
    withObject "datablock" $
    \o -> do
      data'' <- o .: "data"
      summary' <- o .:? "summary"
      icon' <- o .:? "icon"
      return
        DataBlock
        { data' = data''
        , summary = summary'
        , icon = icon'
        }
