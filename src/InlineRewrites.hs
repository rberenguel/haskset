{-# LANGUAGE OverloadedStrings #-}

module InlineRewrites where

import Control.Monad.Identity (Identity)
import Data.Text (pack, splitOn, unpack)
import qualified Data.Text as T
import SlideParser (referenceParser)
import qualified Text.Parsec as Parsec

-- The following parsers are rewriters for special formatting inlined in headers

emphParser :: Parsec.ParsecT String () Identity String
emphParser = do
  Parsec.choice [Parsec.try (Parsec.string "__"), Parsec.try (Parsec.string "_")]

headerInlinedEmph :: Parsec.ParsecT String () Identity T.Text
headerInlinedEmph = do
  start <- Parsec.many Parsec.alphaNum
  emph1 <- emphParser
  middle <- Parsec.many Parsec.alphaNum
  emph2 <- emphParser
  end <- Parsec.manyTill Parsec.alphaNum Parsec.eof
  return (pack (start ++ " " ++ emph1 ++ middle ++ emph2 ++ " " ++ end))

reformatInlinedEmph :: T.Text -> T.Text
reformatInlinedEmph text = T.intercalate " " (map rightParse (splitOn " " text))
  where
    rightParse text = case (Parsec.parse (Parsec.choice [Parsec.try headerInlinedEmph, Parsec.try anyUntilEof]) "" (unpack text)) of
      Right thing -> thing
      Left _ -> "[Failed parsing inlined formats]"

inlinedReference :: Parsec.ParsecT String () Identity T.Text
inlinedReference = do
  start <- Parsec.many Parsec.alphaNum
  counter <- referenceParser
  end <- Parsec.manyTill Parsec.alphaNum Parsec.eof
  return (pack (start ++ "<sup class='superscript'>" ++ show counter ++ "</sup>" ++ end))

-- Footnotes need to be processed aside to add superscript capabilities, Reveal.js seems to have
-- a bug with Pandoc-exported footnotes. This is not ideal

reformatFootnote :: T.Text -> T.Text
reformatFootnote text = T.intercalate " " (map rightParse (splitOn " " text))
  where
    rightParse tex = case inlinedParser "" (unpack tex) of
      Right thing -> thing
      Left _ -> "[Failed parsing inlined reference in header]"
    inlinedParser = Parsec.parse (Parsec.choice [Parsec.try inlinedReference, Parsec.try anyUntilEof])

anyUntilEof :: Parsec.ParsecT String () Identity T.Text
anyUntilEof = do
  stuff <- Parsec.manyTill Parsec.anyChar Parsec.eof
  return (pack stuff)
