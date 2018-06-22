{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Refactorio.Legacy where

import           Refactorio.Prelude             as P   hiding ( (<>) )
import qualified Streaming.Prelude              as S

import qualified Data.List                      as L
import qualified Data.Set                       as Set
import qualified Data.Text                      as T
import           Language.Haskell.Exts
import           Language.Haskell.Interpreter
import           Refactorio.FilenameFilter
import           Refactorio.Theme
import           Refactorio.Types
import           System.Posix.Files
import           X.Language.Haskell.Exts.Prisms               ( ourParseMode )
import           X.Language.Haskell.Interpreter               ( build )
import           X.Rainbow
import           X.Streaming.Files                            ( FileInfo
                                                              , tree
                                                              )

replace :: Config -> Text -> IO ()
replace Config{..} _mapFnSrc = panic "legacyReplace not implemented yet"

search :: Config -> IO ()
search config@Config{..} = legacySearchWith config f . unTarget $ target
  where
    f               = panic "legacySearch f undefined"
    allFilters      = expandExtraFilters specialModeMay filenameFilters
    compiledFilters = map compileFilter . Set.toList $ allFilters

-- DRY UP vs Engine
-- TODO: read .*ignore files from the target dir down to the current file, caching
--       along the way, etc. but for now...
ignored :: FilePath -> Bool
ignored path = ocd
  || path `contains`   ".stack-work"
  || path `startsWith` ".git/"
  || path `contains`   "/.git/"
  where
    ocd = False

legacySearchWith :: Config
                 -> ATraversal' (Module SrcSpanInfo) SrcSpanInfo
                 -> FilePath
                 -> IO ()
legacySearchWith config@Config{..} _trav _path = do
  putChunksLn
    [ chunk within & withinHdr theme
    , chunk (T.pack . unTarget $ target) & withinValue theme
    ]
  putChunksLn
    [ chunk query & searchHdr theme
    , chunk (unExpression expr) & searchValue theme
    ]
  nl
  searchByLens config
   where
     within    = "Searching within: "
     query     = justify "  for matches to: "
     justify s = T.replicate (T.length within - T.length s) " " <> s
     theme     = defaultTheme

searchByLens :: Config -> IO ()
searchByLens Config {..} = makeLens (unExpression expr) >>= \case
  Left err -> print err
  Right trav -> S.mapM_ (showMatches trav)
    . S.chain reportFile
    . S.filter (\(p, _) -> ".hs" `L.isSuffixOf` p && not (".stack-work" `L.isInfixOf` p))
    . tree
    . unTarget
    $ target
    where
      showMatches t (p, _) = findMatches t p
                               >>= mapM_ (\x -> printPrettily theme x >> nl)

      reportFile :: FileInfo -> IO ()
      reportFile (p, _) = putChunkLn (chunk (T.pack p) & filename theme)
      theme = defaultTheme

makeLens :: Text -> IO (Either InterpreterError
                        (ATraversal' (Module SrcSpanInfo) SrcSpanInfo))
makeLens = build preludes
  where
    preludes = ["Refactorio.Prelude.Haskell"]

findMatches :: ATraversal' (Module SrcSpanInfo) SrcSpanInfo -> FilePath -> IO [SrcSpanInfo]
findMatches trav path = do
  sourceString <- unpack <$> readFile path
  case parseFileContentsWithMode (ourParseMode { parseFilename = path }) sourceString of
    ParseFailed srcLoc' err -> panic $ "ERROR at " <> show srcLoc' <> ": " <> show err
    ParseOk parsedMod       -> return $ toListOf (cloneTraversal trav) parsedMod

printPrettily :: Theme -> SrcSpanInfo -> IO () -- TODO: don't read the file each time
printPrettily theme spanInfo = putColorFrom theme span =<< readFile (srcSpanFilename span)
    where
      span = spanInfo ^. srcSpanInfoL

srcSpanInfoL :: Lens' SrcSpanInfo SrcSpan
srcSpanInfoL = lens srcInfoSpan $ \ssi sis -> ssi { srcInfoSpan = sis }

putColorFrom :: Theme -> SrcSpan -> Text -> IO ()
putColorFrom theme span src = do
  mapM_ putStr
    . chunksToByteStrings toByteStringsColors256
    . chunkify
    . L.take (srcSpanEndLine span - srcSpanStartLine span + 1)
    . L.drop (srcSpanStartLine span - 1)
    . T.lines
    $ src
  putStr ("\n" :: ByteString)
  where
    chunkify :: [Text] -> [Chunk Text]
    chunkify = \case
      []     -> panic "how can we have no lines?"
      [x]    -> [pre, val, suf]
        where
          pre :: Chunk Text
          pre = (chunk . T.take startc $ x)

          val :: Chunk Text
          val = (chunk . T.take n . T.drop startc $ x)
            & match theme
            where
              n = endc - startc

          suf :: Chunk Text
          suf = (chunk . T.drop endc $ x)

          startc = srcSpanStartColumn span - 1
          endc   = srcSpanEndColumn span - 1

      xs@(x:_) -> firstLine ++ middleLines ++ lastLine
        where
          firstLine = [pre, firstVal]

          middleLines                        -- TODO: we need 'total' concentwation
            | L.length xs <  2 = panic "unpossible!"
            | L.length xs == 2 = []
            | otherwise = case initMay xs of
                            Nothing  -> panic "initMay failing - unpossible!"
                            Just xs' -> fmap chunk . L.drop 1 $ xs'

          lastLine    = [lastVal, post]

          pre      = chunk $ T.take (srcSpanStartColumn span) x
          firstVal = chunk $ T.drop (srcSpanStartColumn span) x
          lastVal  = chunk $ T.take (srcSpanEndColumn span)   lx
          post     = chunk $ T.drop (srcSpanEndColumn span)   lx

          lx = fromMaybe (panic "unpossible!") . lastMay $ xs
