module ArgumentParserSpec
  ( argumentParserSpec
  ) where

import DarkSky.App.Config
import DarkSky.App.Config.ArgumentParser
import DarkSky.Types (Coordinate(..))
import Options.Applicative
import Test.Hspec

argumentParserSpec :: IO ()
argumentParserSpec =
  hspec $
  describe "ArgumentParser" $
  do describe "succeeds" $
       do it "full" $
            runConfigParser ["--key", "key123", "--lat", "1", "--long", "2"] `shouldBe`
            Just (Config (Just "key123") (Just (Coordinate 1 2)))
          it "empty" $
            runConfigParser [] `shouldBe` Just (Config Nothing Nothing)
     describe "fails" $
       do it "invalid latitude" $
            runConfigParser
              ["--key", "key123", "--lat", "gibberish", "--long", "2"] `shouldBe`
            Nothing
          it "invalid longitude" $
            runConfigParser
              ["--key", "key123", "--lat", "1", "--long", "gibberish"] `shouldBe`
            Nothing
          it "invalid argument" $
            runConfigParser ["gibberish"] `shouldBe` Nothing

runConfigParser :: [String] -> Maybe Config
runConfigParser = getParseResult . execParserPure defaultPrefs argumentParser
