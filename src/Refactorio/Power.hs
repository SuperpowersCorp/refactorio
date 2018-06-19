{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Refactorio.Power where

import           Refactorio.Prelude             as P   hiding ( (<>) )
import qualified Streaming.Prelude              as S

import Language.Haskell.Exts
import qualified Data.Set                       as Set
import           Refactorio.FilenameFilter
import           Refactorio.Types
import           System.Posix.Files
import           X.Streaming.Files                            ( tree )

-- CURRENT TARGET: refio --haskell '& __Module.biplate._Int +~ 32'

replace :: Config -> Text -> IO ()
replace Config{..} _mapFnSrc = panic "powerReplace not implemented yet"

search :: Config -> IO ()
search Config{..} = S.mapM_ ( powerSearchWith f )
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

powerSearchWith :: (ATraversal' (Module SrcSpanInfo) SrcSpanInfo) -> FilePath -> IO ()
powerSearchWith = panic "powerSearchWith undefined"

-- +searchOrReplace :: Config -> IO ()
-- +searchOrReplace config@Config{..} = do
--    putChunksLn
-- -    [ chunk "Searching within: " & withinHdr theme
-- +    [ chunk within & withinHdr theme
--      , chunk (pack projectRoot) & withinValue theme
--      ]
--    putChunksLn
-- -    [ chunk "  for matches to: " & searchHdr theme
-- +    [ chunk query & searchHdr theme
--      , chunk lensText & searchValue theme
--      ]
--    newLine
-- -  searchByLens config
-- +  results
-- +  where
-- +    within    = case mapFnSrc of
-- +      "" -> "Searching within: "
-- +      _  -> "Previewing replacement within: "
-- +    query     = justify "  for matches to: "
-- +    justify s = Text.replicate (Text.length within - Text.length s) " " <> s
-- +    results   = case mapFnSrc of
-- +      ""  -> Search.byLens config
-- +      src -> case compileMapFn src of
-- +        Left err -> Replace.displayError err
-- +        Right f  -> Replace.withLens config f
