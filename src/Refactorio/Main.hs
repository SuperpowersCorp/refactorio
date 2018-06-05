{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Refactorio.Main
     ( main
     ) where

import Refactorio.Prelude  hiding ( (<>) )

import Data.Text
import Options.Applicative
import Rainbow.Extra       hiding ( (&) )
import Refactorio.Config
import Refactorio.Lenses
import Refactorio.Theme

main :: IO ()
main = void $ execParser opts >>= performSearch
  where
    opts = info (parser <**> helper) mempty

    parser :: Parser Config
    parser = Config
      <$> strOption ( long "directory"
                      <> short 'd'
                      <> metavar "PROJECT_ROOT"
                      <> value "."
                    )
      <*> ( pack <$> argument str ( metavar "TRAVERSAL" )  )
      <*> pure defaultTheme
      <*> pure NoMapFn
      <*> pure View

performSearch :: Config -> IO ()
performSearch config@Config {..} = do
  putChunksLn
    [ chunk "Searching within: " & withinHdr theme
    , chunk (pack projectRoot) & withinValue theme
    ]
  putChunksLn
    [ chunk "  for matches to: " & searchHdr theme
    , chunk lensText & searchValue theme
    ]
  newLine
  searchByLens config
