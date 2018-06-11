{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Refactorio.Main
     ( main
     ) where

import Refactorio.Prelude  as P       hiding ( (<>)
                                             , replace
                                             )

import Data.Text           as Text    hiding ( replace )
import Options.Applicative
import Rainbow.Extra                  hiding ( (&) )
import Refactorio.Config
import Refactorio.Replace  as Replace
import Refactorio.Search   as Search
import Refactorio.Theme

main :: IO ()
main = void $ execParser opts >>= searchOrReplace
  where
    opts = info (parser <**> helper) $ fullDesc
           <> header "Refactorio - Optical Refactoring Tool"
           <> progDesc "What does this thing do?"

parser :: Parser Config
parser = Config <$> directoryOpt <*> traversalOpt <*> themeOpt <*> functionOpt
  where
    -- TODO: figure out why showDefault's aren't working
    directoryOpt = strOption $ long "directory"
                            <> short 'd'
                            <> metavar "PROJECT-ROOT"
                            <> value "."
                            <> showDefault
    traversalOpt = pack <$> argument str ( metavar "TRAVERSAL" )
    themeOpt     = pure defaultTheme -- TODO
    functionOpt  = pack <<$>> strOption $ long "fmap"
                           <> short 'f'
                           <> metavar "FUNCTION-SOURCE"
                           <> showDefault
                           <> value ""

searchOrReplace :: Config -> IO ()
searchOrReplace config@Config{..} = do
  putChunksLn
    [ chunk within & withinHdr theme
    , chunk (pack projectRoot) & withinValue theme
    ]
  putChunksLn
    [ chunk query & searchHdr theme
    , chunk lensText & searchValue theme
    ]
  newLine
  results
  where
    within    = case mapFnSrc of
      "" -> "Searching within: "
      _  -> "Previewing replacement within: "
    query     = justify "  for matches to: "
    justify s = Text.replicate (Text.length within - Text.length s) " " <> s
    results   = case mapFnSrc of
      ""  -> Search.byLens config
      src -> case compileMapFn src of
        Left err -> Replace.displayError err
        Right f  -> Replace.withLens config f
