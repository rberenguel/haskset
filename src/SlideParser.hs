{-# LANGUAGE OverloadedStrings #-}

module SlideParser where

import Control.Monad (guard)
import Control.Monad.Identity (Identity)
import Data.Char (digitToInt)
import Data.Text (pack, splitOn, strip, unpack)
import qualified Debug.Trace as Debug
import qualified Data.Text as T
import qualified Text.Parsec as Parsec

-- TODO: Can't parse if there are no new lines at
-- the end of text blocks/headers and the like (i.e. lone header in "slide" fails)

-- This is a partial Markdown parser, suited for parsing
-- Deckset-formatted slides. Some Deckset features are
-- not supported (some may be added as I see fit)

data Object
  = Header
      { header :: T.Text,
        text :: T.Text
      }
  | Footnote {ref :: Int, text :: T.Text}
  | Reference {ref :: Int}
  | Speakernote {text :: T.Text}
  | Blank
  | Image
      { position :: [Directive],
        location :: T.Text
      }
  | Text
      { text :: T.Text
      }
  | UListItem
      { listText :: T.Text
      }
  | OListItem
      { listText :: T.Text
      }
  | List
      { items :: [Object]
      }
  | Setup {rule :: Directive, flag :: Bool}
  deriving (Show)

data Directive = FloatLeft | FloatRight | Fit | Empty | Centering | Failed | BuildLists deriving (Show, Eq)

newtype Slide = Slide [Object] deriving (Show)

headerParser :: Parsec.ParsecT String () Identity Object
headerParser = do
  Parsec.spaces
  header <- pack <$> Parsec.many1 (Parsec.char '#')
  Parsec.char ' '
  title <- pack <$> Parsec.manyTill Parsec.anyChar newLineTryParser
  return (Header header title)

newLineTryParser :: Parsec.ParsecT String () Identity Object
newLineTryParser = do
  Parsec.try (Parsec.char '\n')
  return Blank

spaceTryParser :: Parsec.ParsecT String () Identity Object
spaceTryParser = do
  Parsec.try Parsec.spaces
  return Blank

referenceParser :: Parsec.ParsecT String () Identity Int
referenceParser = do
  Parsec.try $ Parsec.string "[^"
  count <- fmap digitToInt (Parsec.try Parsec.digit)
  Parsec.try $ Parsec.char ']'
  return count

footNoteParser :: Parsec.ParsecT String () Identity Object
footNoteParser = do
  Parsec.try startOfLine
  count <- Parsec.try referenceParser
  Parsec.try $ Parsec.string ": "
  fmap (Footnote count . pack) (Parsec.manyTill Parsec.anyChar Parsec.eof)

speakerNoteParser :: Parsec.ParsecT String () Identity Object
speakerNoteParser = do
  Parsec.spaces
  Parsec.try startOfLine
  Parsec.try $ Parsec.char '^'
  fmap (Speakernote . pack) (Parsec.manyTill Parsec.anyChar Parsec.eof)

startOfLine :: Monad m => Parsec.ParsecT s u m ()
startOfLine = do
  pos <- Parsec.getPosition
  guard (Parsec.sourceColumn pos == 1)

listParser :: Parsec.ParsecT String () Identity Object
listParser = do
  fmap
    List
    ( Parsec.choice
        [ Parsec.many1 (Parsec.try ulistParser),
          Parsec.many1 (Parsec.try olistParser)
        ]
    )

ulistParser :: Parsec.ParsecT String () Identity Object
ulistParser = do
  startOfLine
  Parsec.try $ Parsec.string "- "
  fmap (UListItem . pack) (Parsec.manyTill Parsec.anyChar newLineTryParser)

olistParser :: Parsec.ParsecT String () Identity Object
olistParser = do
  startOfLine
  Parsec.try $ Parsec.digit *> Parsec.string ". "
  fmap (OListItem . pack) (Parsec.manyTill Parsec.anyChar newLineTryParser)

setupParser :: Parsec.ParsecT String () Identity Object
setupParser = do
  Parsec.spaces
  Parsec.try $ Parsec.string "[."
  obj <- Parsec.manyTill Parsec.anyChar (Parsec.try $ Parsec.char ':')
  flag <- Parsec.manyTill Parsec.anyChar (Parsec.try $ Parsec.char ']')
  return (Setup (parse obj) (bool flag))
  where
    -- at the moment it doesn't check value of the rule, assumes true

    parse "build-lists" = BuildLists
    parse _ = Empty
    bool "True" = True
    bool _ = False

imageParser :: Parsec.ParsecT String () Identity Object
imageParser = do
  Parsec.spaces
  Parsec.try (Parsec.string "![")
  position <- Parsec.manyTill Parsec.anyChar (Parsec.char ']')
  Parsec.char '('
  location <- Parsec.manyTill Parsec.anyChar (Parsec.char ')')
  return (Image (parse (pack position)) (pack location))
  where
    parse pos = map (directive . strip) (splitOn "," pos)
    directive dir = case dir of
      "fit" -> Fit
      "right" -> FloatRight
      "left" -> FloatLeft
      "" -> Empty
      _ -> Failed

nonText :: Parsec.ParsecT String () Identity Object
nonText =
  Parsec.choice $
    Parsec.try
      <$> [ footNoteParser,
            speakerNoteParser,
            listParser,
            setupParser,
            headerParser,
            imageParser,
            newLineTryParser
          ]

textParser :: Parsec.ParsecT String () Identity Object
textParser = fmap (Text . pack) (Parsec.manyTill Parsec.anyChar nonText)

oneOfSlide :: Parsec.ParsecT String () Identity Object
oneOfSlide = do
  Parsec.choice $
    Parsec.try
      <$> [ headerParser,
            footNoteParser,
            speakerNoteParser,
            listParser,
            setupParser,
            imageParser,
            textParser,
            newLineTryParser,
            spaceTryParser
          ]

debug = flip Debug.trace

slideParser :: Parsec.ParsecT String () Identity Slide
slideParser = do
  Parsec.spaces
  Slide <$> Parsec.manyTill oneOfSlide Parsec.eof

textToSlides :: T.Text -> [Either Parsec.ParseError Slide]
textToSlides text = map parser splat
  where
    splat = splitOn "\n---\n" text
    parser txt = (Parsec.parse slideParser "" . unpack) txt `debug` unpack txt
