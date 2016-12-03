module BlockTypeSpec
  ( blockTypeSpec
  ) where

import DarkSky.Request.BlockType
import Test.Hspec

blockTypeSpec :: IO ()
blockTypeSpec =
  hspec $
  describe "BlockTypes" $
  it "generates a string" $
  fromBlockType <$>
  [Currently, Minutely, Hourly, Daily, Alerts, Flags] `shouldBe`
  ["currently", "minutely", "hourly", "daily", "alerts", "flags"]
