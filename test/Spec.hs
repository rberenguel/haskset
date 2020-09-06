{-# LANGUAGE OverloadedStrings #-}

import Data.Text (unpack)
import qualified Data.Text.IO as TT
import qualified Images as I
import SlideConverter
import SlideParser
import qualified Splits as S
import Test.Hspec
import qualified Text as T
import qualified Text.Parsec as Parsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parsing of slides" $ do
    it "should handle images on either side, with or without fit" $ do
      mapM_ testCase I.cases
    it "should handle splits of up to 5 images" $ do
      mapM_ testCase S.cases
    it "should handle special text rules" $ do
      mapM_ testCase T.cases
  where
    testCase (slide, expected) = do
      let text = parsedSlideToText parsed
          parsed = (Parsec.parse slideParser "" . unpack) slide
      TT.putStrLn text
      print parsed
      text `shouldBe` expected
