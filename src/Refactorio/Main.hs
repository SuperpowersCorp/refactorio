{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Refactorio.Main ( main ) where

import           Refactorio.Prelude        as P    hiding ( (<>) )

import qualified Data.Set                  as Set
import qualified Data.Text                 as Text
import           Options.Applicative               hiding ( prefs )
import           Refactorio.Engine                        ( process )
import           Refactorio.FilenameFilter
import           Refactorio.Types
import           X.Rainbow

-- CURRENT TARGET:   refio --haskell view "__Module.biplate._Int" & "(+32)"

main :: IO ()
main = void $ customExecParser prefs opts >>= process . expandExtraFilters
  where
    prefs = defaultPrefs
      { prefShowHelpOnError = True
      , prefShowHelpOnEmpty = True
      }

    opts = info (parser <**> helper) $ fullDesc
           <> header   "Refactorio - Optical Refactoring Tool"
           <> progDesc "Zen and the art of optical file maintenance."

expandExtraFilters :: Config -> Config
expandExtraFilters config@Config{..} = config

parser :: Parser Config
parser = prefixConfigParser
  where
    prefixConfigParser :: Parser Config
    prefixConfigParser = Config
      <$> filenameFilterSetParser
      <*> expressionParser
      <*> optional specialModeParser
      <*> updateModeParser
      <*> targetParser

specialModeParser :: Parser SpecialMode
specialModeParser =
  Haskell <$ switch ( long "haskell"
                   <> help "Include .hs files and activate Haskell module parsing mode."
                    )
  <|> JSON <$ switch ( long "json"
                    <> help "Include .json files."
                     )
  <|> YAML <$ switch ( long "yaml"
                    <> help "Include .yaml or .yml files."
                     )

updateModeParser :: Parser UpdateMode
updateModeParser =
  AskMode <$ switch ( long "ask"
                   <> short 'a'
                   <> help "Ask before changing files (default)"
                    )
  <|> PreviewMode <$ switch ( long "preview"
                           <> short 'p'
                           <> help "Only show what changes would be made"
                            )
  <|> ReviewMode <$ switch ( long "review"
                          <> short 'r'
                          <> help "Make the changes and show what was changed"
                           )
  <|> JustDoItMode <$ switch ( long "just-do-it"
                            <> short 'j'
                            <> help "Make the changes and only report changed filenames"
                             )
  <|> pure AskMode

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
 <> showDefault
 <> value       "/tmp/voltron" -- TODO
  )

filenameFilterSetParser :: Parser (Set FilenameFilter)
filenameFilterSetParser = Set.fromList . map (FilenameFilter . Text.pack) <$>
  many ( strOption ( long    "glob"
                     <> short   'g'
                     <> metavar "GLOB"
                     <> help    "Glob matches to include (eg '*.ini', 'f??b?r.c')"
                   )
       )
