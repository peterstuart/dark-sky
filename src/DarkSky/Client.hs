{-# LANGUAGE OverloadedStrings #-}

module DarkSky.Client
  ( getForecast
  , httpRequest
  ) where

import DarkSky.Request
import DarkSky.Response (Response)
import Data.ByteString.Char8 (pack)
import Network.HTTP.Simple as HTTP

getForecast :: DarkSky.Request.Request -> IO DarkSky.Response.Response
getForecast request = getResponseBody <$> httpJSON (httpRequest request)

httpRequest :: DarkSky.Request.Request -> HTTP.Request
httpRequest r =
  setRequestQueryString queryParameters .
  setRequestPath (pack $ path r) .
  setRequestHost "api.darksky.net" . setRequestPort 443 . setRequestSecure True $
  defaultRequest
  where
    queryParameters = convert <$> parameters r
    convert (key', value') = (pack key', Just $ pack value')
