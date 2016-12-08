import AlertSpec
import ArgumentParserSpec
import BlockTypeSpec
import ConfigSpec
import DataBlockSpec
import DataPointSpec
import FlagsSpec
import IconSpec
import PrecipitationTypeSpec
import RequestSpec
import ResponseSpec
import UnitSpec

main :: IO ()
main = do
  responseSpecs
  requestSpecs
  appSpecs

responseSpecs :: IO ()
responseSpecs = do
  alertSpec
  flagsSpec
  iconSpec
  precipitationTypeSpec
  unitSpec
  dataPointSpec
  dataBlockSpec
  responseSpec

requestSpecs :: IO ()
requestSpecs = do
  blockTypeSpec
  requestSpec

appSpecs :: IO ()
appSpecs = do
  configSpec
  argumentParserSpec
