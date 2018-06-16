{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Refactorio.Main ( main ) where

import Refactorio.Prelude  as P       hiding ( (<>) )


import Data.Set as Set
import Data.Text as Text
import Options.Applicative
import Refactorio.Config
import Refactorio.Replace  as Replace
import Refactorio.Search   as Search
import Refactorio.Theme
import X.Rainbow                      hiding ( (&) )

-- CURRENT TARGET:   refio . --haskell view "__Module.biplate._Int" --pre-mqp "+32"

main :: IO ()
main = do
  putLn "DANGER ZONE CONSTRUCTION IN PROGRESS.."
  void $ execParser opts >>= makeTheMagicHappen
  where
    opts = info (parser <**> helper) $ fullDesc
           <> header "Refactorio - Optical Refactoring Tool"
           <> progDesc "Zen and the art of optical file maintenance."

    makeTheMagicHappen :: (Config, CommonConfig) -> IO ()
    makeTheMagicHappen (ConfigExecute config, commonConfig) =
      Replace.execute commonConfig config
    makeTheMagicHappen (ConfigPreview config, commonConfig) =
      Replace.preview commonConfig config
    makeTheMagicHappen (ConfigView config, commonConfig) =
      Search.view commonConfig config

parser :: Parser (Config, CommonConfig)
parser = flip (,) <$> commonConfigParser <*> configParser

commonConfigParser :: Parser CommonConfig
commonConfigParser = CommonConfig
  <$> ( Set.fromList <$> filenameFilterParser )
  <*> ( Target <$> strOption
        ( long "target"
       <> short 't'
       <> metavar "TARGET"
       <> help "a file/directory to search/replace"
       <> showDefault
       <> value "."
        )
      )
  <*> pure defaultTheme

filenameFilterParser :: Parser [FilenameFilter]
filenameFilterParser = pure [DotPattern "-hs"] -- TODO

configParser :: Parser Config
configParser = subparser
  (  stdCmd "replace" replaceOpts                 "Optical Replace"
  <> stdCmd "preview" previewOpts "(Preview of...) Optical Replace"
  <> stdCmd "view"    viewOpts                    "Optical Search"
  )
  where
    stdCmd l x d = command l ( stdInfo x d )
    stdInfo x d  = info x    ( progDesc d  )

    viewOpts :: Parser Config
    viewOpts = ConfigView . SearchConfig . LensText <$> traversalArg

    traversalArg :: Parser Text
    traversalArg = pack <$> argument str
       ( metavar "TRAVERSAL"
      <> help    "Traversal' FileInfo SrcSpanInfo"
       )

    previewOpts :: Parser Config
    previewOpts = ConfigPreview <$> replaceConfigParser

    replaceOpts :: Parser Config
    replaceOpts = ConfigExecute <$> replaceConfigParser

    replaceConfigParser :: Parser ReplaceConfig
    replaceConfigParser =
      ( ReplaceConfig
        <$> ( LensText <$> traversalArg)
        <*> operatorArg
        <*> ( MapFnText
              <$> ( pack <$> argument str
                    ( metavar "F"
                   <> help    "Haskell function of type ':: a -> b'"
                    )))
      )

    operatorArg :: Parser LensOperator
    operatorArg = panic "operatorArg undefined"
