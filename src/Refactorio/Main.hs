{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Refactorio.Main ( main ) where

import           Refactorio.Prelude        as P    hiding ( (<>) )

import qualified Data.List.NonEmpty        as NE
import qualified Data.Set                  as Set
import qualified Data.Text                 as Text
import           Options.Applicative               hiding ( prefs )
import           Refactorio.Engine                        ( process )
import           Refactorio.FilenameFilter
import           Refactorio.SpecialMode
import           Refactorio.Types
import           X.Rainbow

main :: IO ()
main = void $ customExecParser prefs opts >>= process . wrapSrc
  where
    prefs = defaultPrefs
      { prefShowHelpOnError = True
      , prefShowHelpOnEmpty = True
      }

    opts = info (parser <**> helper) $ fullDesc
      <> header "Refactorio - Optical Refactoring Tool"

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
      <$> monadicParser
      <*> expressionParser
      <*> targetParser
      <*> filenameFilterSetParser
      <*> optional preludeParser
      <*> updateModeParser
      <*> specialModeParser

    -- So Optparse Applicative will generate the options in our preferred order
    reorder mo ex ta ff pr um sp = Config ff ex mo pr sp um ta

monadicParser :: Parser Bool
monadicParser = switch
  ( long "io"
 <> help "Add IO to expr type ('ByteString -> IO ByteString')"
  )

expressionParser :: Parser Expression
expressionParser = Expression . Text.pack <$> argument str
  ( metavar "EXPR"
 <> help    "A Haskell expression of type 'ByteString -> ByteString'"
  )

targetParser :: Parser (NonEmpty Target)
targetParser =
  ( NE.fromList
    <$> some ( Target <$> strOption
               ( long    "target"
              <> short   't'
              <> metavar "TARGET"
              <> help    "One or more files/directories to traverse (defaults to stdin, use '-' to force stdin)"
               )
             )
  ) <|> (pure . pure) defaultTarget
  where
    defaultTarget = Target "-"

filenameFilterSetParser :: Parser (Set FilenameFilter)
filenameFilterSetParser = Set.fromList . map (FilenameFilter . Text.pack) <$> many
  ( strOption ( long    "glob"
             <> short   'g'
             <> metavar "GLOB"
             <> help    "Glob matches to include (eg '*.ini', 'f??b?r.c')"
              ) )

preludeParser :: Parser FilePath
preludeParser = strOption
  ( long    "prelude"
 <> help    "Use a specific Prelude module"
 <> metavar "MODULE"
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
  <|> ModifyMode <$ switch ( long  "modify"
                          <> short 'm'
                          <> help  "Make the changes and summarize changed filenames"
                           )
  -- <|> ReplaceMode <$ switch ( long  "replace"
  --                          <> short 'r'
  --                          <> help  "Activate alternate replace mode (Haskell mode only currently)"
  --                           )
  <|> ReviewMode <$ switch ( long  "review"
                          <> help  "Make the changes and show details of changes"
                           )
  <|> SearchMode <$ switch ( long  "search"
                          <> short 's'
                          <> help  "Activate alternate search mode (Haskell mode only right now)"
                           )
  <|> pure AskMode

specialModeParser :: Parser (Maybe SpecialMode)
specialModeParser =
  langSwitch Haskell ( long "haskell"
                    <> long "hs"
                    <> help "Include .hs files and make Haskell ops available"
                     )
  <|> langSwitch Html ( long "html"
                     <> help "Include .htm(l) files and make XML ops available"
                      )
  <|> langSwitch JavaScript ( long "javascript"
                           <> long "js"
                           <> help "Include .js files and make JavaScript ops available"
                            )
  <|> langSwitch C ( long "c"
                  <> help "Include .c files and make C ops available"
                   )
  <|> langSwitch Json ( long "json"
                     <> help "Include .json files and make JSON ops available"
                      )
  <|> langSwitch Xml ( long "xml"
                    <> help "Include .xml files and make XML ops available"
                     )
  <|> langSwitch Yaml ( long "yaml"
                     <> help "Include .y(a)ml files and make YAML ops available"
                      )
  where
    langSwitch m = (mmap m <$>) . switch

    mmap :: SpecialMode -> Bool -> Maybe SpecialMode
    mmap sm True  = Just sm
    mmap _  False = Nothing
