{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Refactorio.Power where

import           Refactorio.Prelude        as P   hiding ( (<>) )
import qualified Streaming.Prelude         as S

import qualified Data.Set                  as Set
import           Language.Haskell.Exts
import           Refactorio.FilenameFilter
import           Refactorio.Types
import           System.Posix.Files
import           X.Streaming.Files                       ( tree )

-- import           X.Rainbow

-- CURRENT TARGET: refio --haskell '& __Module.biplate._Int +~ 32'

replace :: Config -> Text -> IO ()
replace Config{..} _mapFnSrc = panic "powerReplace not implemented yet"

search :: Config -> IO ()
search config@Config{..} = S.mapM_ ( powerSearchWith config f )
  . S.filter ( matchesAny compiledFilters )
  . S.filter ( not . ignored )
  . S.map fst
  . S.filter ( not . isDirectory . snd )
  . tree
  . unTarget
  $ target
  where
    f               = panic "powerSearch f undefined"
    allFilters      = expandExtraFilters specialModeMay filenameFilters
    compiledFilters = map compileFilter . Set.toList $ allFilters
    ignored         = panic "ignored undefined"

powerSearchWith :: Config
                -> (ATraversal' (Module SrcSpanInfo) SrcSpanInfo)
                -> FilePath
                -> IO ()
powerSearchWith _config _trav _path = panic "powerSearchWith not impl yet!"

-- powerSearchWith _config _trav _path = do
--   putChunksLn
--     [ chunk within & withinHdr theme
--     , chunk (pack projectRoot) & withinValue theme
--     ]
--   putChunksLn
--     [ chunk query & searchHdr theme
--     , chunk lensText & searchValue theme
--     ]
--   newLine
--   searchByLens config
--   results
--    where
--      within    = case mapFnSrc of
--        "" -> "Searching within: "
--        _  -> "Previewing replacement within: "
--      query     = justify "  for matches to: "
--      justify s = Text.replicate (Text.length within - Text.length s) " " <> s
--      results   = panic "results not impl yet!"
--      -- results   = case mapFnSrc of
--      --   ""  -> Search.byLens config
--      --   src -> case compileMapFn src of
--      --     Left err -> Replace.displayError err
--      --     Right f  -> Replace.withLens config f
