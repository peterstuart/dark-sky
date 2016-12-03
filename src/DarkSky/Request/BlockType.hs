{-# LANGUAGE OverloadedStrings #-}

module DarkSky.Request.BlockType where

data BlockType
  = Currently
  | Minutely
  | Hourly
  | Daily
  | Alerts
  | Flags
  deriving (Eq, Ord, Show)

fromBlockType :: BlockType -> String
fromBlockType Currently = "currently"
fromBlockType Minutely = "minutely"
fromBlockType Hourly = "hourly"
fromBlockType Daily = "daily"
fromBlockType Alerts = "alerts"
fromBlockType Flags = "flags"
