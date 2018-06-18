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
main = void $ customExecParser prefs opts >>= process
  where
    prefs = defaultPrefs
      { prefShowHelpOnError = True
      , prefShowHelpOnEmpty = True
      }

    opts = info (parser <**> helper) $ fullDesc
           <> header   "Refactorio - Optical Refactoring Tool"
           <> progDesc "Zen and the art of optical file maintenance."

parser :: Parser Config
parser = prefixConfigParser
  where
    prefixConfigParser :: Parser Config
    prefixConfigParser = Config
      <$> filenameFilterSetParser
      <*> expressionParser
      <*> previewParser
      <*> targetParser

    _ = Config :: Set FilenameFilter
               -> Expression
               -> PreviewMode
               -> Target
               -> Config

    _fromInfix :: Set FilenameFilter
               -> PreviewMode
               -> Expression
               -> Target
               -> Config
    _fromInfix filts = flip (Config filts)

previewParser :: Parser PreviewMode
previewParser = pure PreviewModeEnabled

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
 <> help        "a file/directory to search/replace"
 <> showDefault
 <> value       "/tmp/voltron" -- TODO
  )

filenameFilterSetParser :: Parser (Set FilenameFilter)
filenameFilterSetParser = unite
  <$> many ( strOption ( long    "ext"
                      <> short   'e'
                      <> metavar "EXT"
                      <> help    "file extension to include (eg 'txt', 'c')"
                       )
           )
  <*> switch ( long "haskell"
            <> help "Include .hs files and activate Haskell module mapping."
             )
  <*> switch ( long "json"
            <> help "Include .json files."
             )
  <*> switch ( long "yaml"
            <> help "Include .yaml or .yml files."
             )
  where
    unite ffs hs json yaml = Set.fromList
      $ map DotPattern ffs
      ++ [Haskell | hs]
      ++ [JSON    | json]
      ++ [YAML    | yaml]
