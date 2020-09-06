{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (intercalate, pack, unpack)
import Data.Text.IO (putStrLn)
import qualified Data.Text.IO as TT
import Options.Applicative
import SlideConverter ( parsedSlideToText )
import SlideParser ( textToSlides ) 
import InlineCSS ( baseCSS )
import System.Directory ( getHomeDirectory )
import System.FilePath (joinPath, splitPath, takeBaseName, takeDirectory)
import System.IO hiding (putStrLn)
import System.IO.Temp ( writeSystemTempFile )
import System.Process ( callCommand )
import Prelude hiding (putStrLn)

-- TODO: Configuration file instead of passing the CSS via CLI
-- TODO: Parsing the CSS ¯\﹍(ツ)﹍/¯
data CLIConfig = CLIConfig
  { filename :: Maybe String,
    css :: Maybe String,
    dumpCSS :: Bool
  }

cliConfig :: Parser CLIConfig
cliConfig =
  CLIConfig
    <$> optional
      ( strArgument (metavar "FILEPATH" <> help "Markdown presentation")
      )
    <*> optional
      ( strOption
          ( long "css"
              <> metavar "FILEPATH"
              <> help "Path to CSS"
          )
      )
    <*> switch
      ( long "dumpCSS"
          <> short 'c'
          <> help "Dump base CSS used"
      )

getFullPath :: FilePath -> IO FilePath
getFullPath s = case splitPath s of
  "~/" : t -> joinPath . (: t) <$> getHomeDirectory
  _ -> return s

pandocCommand filename realFilename css destination =
  unpack
    ( intercalate
        " "
        [ "pandoc -t revealjs",
          "--metadata pagetitle='...'",
          pack ("-H " ++ css),
          "-V width=\\\"100%\\\"",
          "-V height=\\\"90%\\\"",
          "-V transition='fade'",
          "--slide-level 1",
          "-V revealjs-url=https://unpkg.com/reveal.js",
          pack filename,
          "-o",
          pack (joinPath [destination, (takeBaseName realFilename ++ ".html")])
        ]
    )

start :: CLIConfig -> IO ()
start (CLIConfig _ _ True) = do
  TT.putStrLn $ pack baseCSS
start (CLIConfig (Just markdownFilePath) Nothing _) = do
  filepath <- getFullPath markdownFilePath
  handle <- openFile filepath ReadMode
  contents <- pack <$> hGetContents handle
  let slides = textToSlides contents
  -- TODO: Verbose logging, showing the parsed slides 
  let reformatted = intercalate "\n" (map parsedSlideToText slides)
  path <- writeSystemTempFile (takeBaseName filepath ++ ".md") (unpack reformatted)
  css <- writeSystemTempFile "basecss" ("<style>\n" ++ baseCSS ++ "</style>")
  -- TODO: Verbose logging, where is the tmp file?
  let destination = takeDirectory filepath
  callCommand (pandocCommand path filepath css destination)
  putStrLn ("Output written to " <> pack (joinPath [destination, (takeBaseName filepath ++ ".html")]))
start _ = do
  putStrLn "This combination of parameters is not valid or something"

main :: IO ()
main = do
  start =<< execParser opts
  where
    opts =
      info
        (cliConfig <**> helper)
        ( fullDesc
            <> progDesc "Converts a Markdown presentation formatted via Deckset into an equivalent presentation using Reveal.js"
            <> Options.Applicative.header "If it hangs, add new lines before slide separators. And use --- as separators!"
        )