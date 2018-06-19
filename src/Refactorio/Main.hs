{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Refactorio.Main ( main ) where

import           Refactorio.Prelude        as P    hiding ( (<>) )

import qualified Data.Set                  as Set
import qualified Data.Text                 as Text
import           Options.Applicative               hiding ( prefs )
import           Refactorio.Engine                        ( process )
import           Refactorio.FilenameFilter
import           Refactorio.Types
import           X.Rainbow

-- CURRENT TARGET: refio --haskell '& __Module.biplate._Int +~ 32'

main :: IO ()
main = void $ customExecParser prefs opts >>= process . wrapSrc
  where
    prefs = defaultPrefs
      { prefShowHelpOnError = True
      , prefShowHelpOnEmpty = True
      }

    opts = info (parser <**> helper) $ fullDesc
           <> header   "Refactorio - Optical Refactoring Tool"
           <> progDesc "Zen and the art of optical file maintenance."

wrapSrc :: Config -> Config
wrapSrc = identity
-- wrapSrc config@Config{..} = case specialModeMay of
--   Nothing -> config
--   Just m  -> case m of
--     Haskell -> config { expr = expr & upon unExpression %~ pre "H.hs" }
--     Json    -> config -- already lens-aeson'able
--     Yaml    -> config { expr = expr & upon unExpression %~ pre "H.yaml" }
--   where
--     pre x y = x <> " . (" <> y <> ")"

parser :: Parser Config
parser = prefixConfigParser
  where
    prefixConfigParser :: Parser Config
    prefixConfigParser = reorder
      <$> expressionParser
      <*> targetParser
      <*> filenameFilterSetParser
      <*> optional preludeParser
      <*> updateModeParser
      <*> specialModeParser

    -- So Optparse Applicative will generate the options in the right order
    reorder ex ta ff pr um sp = Config ff ex pr sp um ta

expressionParser :: Parser Expression
expressionParser = Expression . Text.pack <$> argument str
  ( metavar "EXPR"
 <> help    "ByteString -> ByteString"
  )

targetParser :: Parser Target
targetParser = Target <$> strOption
  ( long        "target"
 <> short       't'
 <> metavar     "TARGET"
 <> help        "A file/directory to search/replace"
 <> value       "."
 <> showDefault
  )

filenameFilterSetParser :: Parser (Set FilenameFilter)
filenameFilterSetParser = Set.fromList . map (FilenameFilter . Text.pack) <$> many
  ( strOption ( long    "glob"
             <> short   'g'
             <> metavar "GLOB"
             <> help    "Glob matches to include (eg '*.ini', 'f??b?r.c')"
              )
  )

preludeParser :: Parser FilePath
preludeParser = strOption
  ( long    "prelude"
 <> help    "Use a specific Prelude"
 <> metavar "PRELUDE"
  )

updateModeParser :: Parser UpdateMode
updateModeParser =
  AskMode <$ switch ( long  "ask"
                   <> short 'a'
                   <> help  "Ask before changing files (default)"
                    )
  <|> PreviewMode <$ switch ( long  "preview"
                           <> short 'p'
                           <> help  "Only show the changes that would be made"
                            )
  <|> ReviewMode <$ switch ( long  "review"
                          <> short 'r'
                          <> help  "Make the changes and show details of changes"
                           )
  <|> ModifyMode <$ switch ( long  "modify"
                          <> short 'm'
                          <> help  "Make the changes and summarize changed filenames"
                           )
  <|> pure AskMode

specialModeParser :: Parser (Maybe SpecialMode)
specialModeParser = resolve <$> ( (,,,,)
  <$> langSwitch Haskell
               ( long "haskell"
              <> long "hs"
              <> help "Include .hs files and make Haskell ops available"
               )
  <*> langSwitch Html
               ( long "html"
              <> help "Include .htm* files"
               )
  <*> langSwitch Json
               ( long "json"
              <> help "Include .json files and make JSON ops available"
               )
  <*> langSwitch Xml
               ( long "xml"
              <> help "Include .xml files and make XML ops available"
               )
  <*> langSwitch Yaml
               ( long "yaml"
              <> help "Include .yaml or .yml files and make YAML ops available"
               )
                                )
  where
    langSwitch m = (mmap m <$>) . switch

    mmap :: SpecialMode -> Bool -> Maybe SpecialMode
    mmap sm True  = Just sm
    mmap _  False = Nothing

    -- allows us to size the tuple arbitrarily
    resolve = head . catMaybes . view (partsOf each)
