{-# LANGUAGE OverloadedStrings #-}

module SlideConverter where

import Data.Text (pack)
import qualified Data.Text as T
import InlineRewrites
import SlideParser
import qualified Text.Parsec as Parsec

-- If there are more than 2 images, they are split
-- as background automatically. Text will be centered
-- in front

imageSplit :: [Object] -> Int -> [T.Text]
imageSplit objs numImages = map formatOne objs
  where
    formatOne (Image position location) =
      T.concat
        [ "<div class='background-multi-image ",
          pack (numToStr numImages),
          "-image overlay' style='background-image: url(",
          location,
          ");'></div>"
        ]
    formatOne _ = ""
    numToStr 2 = "two"
    numToStr 3 = "three"
    numToStr 4 = "four"
    numToStr 5 = "five"
    numToStr _ = "UH"

-- Formatting a single image is based on a set of rules,
-- established on CSS later:

-- If set to either side, half-image-cover and the corresponding
-- position.
-- If additionally we add fit, half-image fit
-- If there is no positioning, fit-image for class, additionally pass fit.
-- Note that this will/should impact the rest of the text in the slide

singleImage :: Object -> T.Text
singleImage (Image directives location) =
  T.concat
    [ "<div class='",
      clazz directives,
      " ",
      position directives,
      "' style=\"background-image: url(",
      location,
      ");\"></div>"
    ]
  where
    clazz [Fit] = "fit-image"
    clazz [_, Fit] = "half-image-fit"
    clazz [Fit, _] = clazz [FloatLeft, Fit]
    clazz (FloatLeft : xs) = "half-image-cover"
    clazz (FloatRight : xs) = "half-image-cover"
    clazz _ = "fit-image"
    position (FloatLeft : xs) = "left " <> position xs
    position (FloatRight : xs) = "right " <> position xs
    position (Fit : xs) = "fit" <> position xs
    position [] = ""
    position unknown = pack ("unknown directives (position): " <> show unknown)
singleImage _ = ""

isImage :: Object -> Bool
isImage obj = case obj of
  Image {} -> True
  _ -> False

isSetup :: Object -> Bool
isSetup obj = case obj of
  Setup {} -> True
  _ -> False

-- When formatting images, if it's one,
-- we need to send text _to the other side_.
-- Can this be part of the return value of
-- formatting one image?

formatSlide :: Slide -> [T.Text]
formatSlide (Slide objs) =
  let images = filter isImage objs
      setups = filter isSetup objs
      setup = map rule setups
      nonImages = filter (not . isImage) objs
      (formattedImages, directives) = case length images of
        x | x > 1 -> (imageSplit images x, [Centering])
        1 -> ([singleImage (head images)], position (head images))
        _ -> ([], [Empty])
      formattedNonImages = map (format setup) nonImages
   in formattedImages ++ wrapInDirectives (directives ++ setup) formattedNonImages

-- Either-side images push text to the other side (via flexbox containers)
-- Fit directive is at the moment mostly ignored, makes no difference to the underlying content
wrap :: T.Text -> [T.Text] -> [T.Text]
wrap clazz text = ["::: " <> clazz] ++ text ++ [":::"]

row :: [T.Text] -> [T.Text]
row = wrap "row"

col :: [T.Text] -> [T.Text]
col = wrap "col"

container :: [T.Text] -> [T.Text]
container = wrap "container"

cRowCol :: [T.Text] -> [T.Text]
cRowCol = container . row . col

wrapInDirectives :: [Directive] -> [T.Text] -> [T.Text]
wrapInDirectives (FloatRight : xs) existing = container (row (wrapInDirectives xs existing))
wrapInDirectives (FloatLeft : xs) existing = container (row [] ++ col (wrapInDirectives xs existing))
wrapInDirectives (Centering : xs) existing =
  ["::: centered-float"]
    ++ wrapInDirectives xs existing
    ++ [":::"]
wrapInDirectives (Fit : xs) existing = cRowCol (wrapInDirectives xs existing)
wrapInDirectives [Empty] existing = cRowCol existing
wrapInDirectives _ existing = existing

-- Formatting converts objects into the adapted textual representation so that Pandoc
-- can format the Reveal presentation properly. I.e. the aside blocks below, or adding
-- Incremental blocks

format :: [Directive] -> Object -> T.Text
format _ (Header level text) = 
  T.concat [level <> "#", 
            " ", 
            reformatFootnote 
            (reformatInlinedEmph text)]
format _ Blank = "\n"
format _ (Text text) = reformatFootnote text
format setup (List items) = case setup of
  (BuildLists : xs) -> T.intercalate "\n" (wrap "incremental" (map (format xs) items))
  any -> T.intercalate "\n" (map (format any) items ++ ["\n"])
format setup (UListItem text) = T.concat ["- ", reformatFootnote text]
format setup (OListItem text) = T.concat ["1. ", reformatFootnote text]
format _ (Speakernote footnote) =
  T.concat
    [ "<aside class='notes'>",
      footnote,
      "</aside>"
    ]
format _ (Footnote counter footnote) =
  T.concat
    [ "<div class='footnote-count'>",
      pack . show $ counter,
      " →",
      "</div>",
      "<div class='footnote'>",
      footnote,
      "</div>"
    ]
format _ Setup {} = ""
format _ _ = "Item not implemented, which is pretty weird"

parsedSlideToText :: Either Parsec.ParseError Slide -> T.Text
parsedSlideToText parsed = case parsed of
  Right slide -> T.intercalate "\n" (formatSlide slide ++ [pack "\n---\n"])
  Left err -> pack ("[Failed parsing slide: " ++ show err ++ " ]")
