{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Refactorio.Main ( main ) where

import           Refactorio.Prelude    as P    hiding ( (<>) )
import qualified Streaming.Prelude     as S

import           Control.Lens                  hiding ( argument
                                                      , preview
                                                      )
import qualified Data.List             as List
import qualified Data.Set              as Set
import qualified Data.Text             as Text
import           Language.Haskell.Exts
import           Options.Applicative           hiding ( prefs )
import           Refactorio.Config
import           System.Posix.Files
import           X.Language.Haskell.Interpreter       ( build )
import           X.Rainbow
import           X.Streaming.Files                    ( FileInfo
                                                      , tree
                                                      )
import           Refactorio.StartingPoint

-- CURRENT TARGET:   refio --haskell view "__Module.biplate._Int" & "(+32)"

main :: IO ()
main = void $ customExecParser prefs opts >>= apprehend
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
      <*> lensOperatorParser
      <*> lensTextParser
      <*> optional mapFnMapParser
      <*> previewParser
      <*> targetParser

    _ = Config :: Set FilenameFilter
               -> LensOperator
               -> LensText
               -> Maybe MapFnText
               -> PreviewMode
               -> Target
               -> Config

    _fromInfix :: Set FilenameFilter
               -> LensText
               -> LensOperator
               -> Maybe MapFnText
               -> PreviewMode
               -> Target
               -> Config
    _fromInfix filts = flip (Config filts)

previewParser :: Parser PreviewMode
previewParser = pure PreviewModeEnabled

lensTextParser :: Parser LensText
lensTextParser = LensText . Text.pack <$> argument str
  ( metavar "TRAVERSAL"
 <> help    "ATraversal' StartingPoint (SrcSpanInfo, a)"
  )

mapFnMapParser :: Parser MapFnText
mapFnMapParser = MapFnText . Text.pack <$> argument str
   ( metavar "F"
  <> help "the function to apply with the lens"
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

lensOperatorParser :: Parser LensOperator
lensOperatorParser = hsubparser
  (  command "over"  (info (pure Over)  ( progDesc "target &  traversal %~ f" ) )
  <> command "plus"  (info (pure Plus)  ( progDesc "target &  traversal +~ f" ) )
  <> command "set"   (info (pure Set)   ( progDesc "target &  traversal .~ f" ) )
  <> command "times" (info (pure Times) ( progDesc "target &  traversal *~ f" ) )
  <> command "view"  (info (pure View)  ( progDesc "target ^. traversal"      ) )
  )

filenameFilterSetParser :: Parser (Set FilenameFilter)
filenameFilterSetParser = pure $ Set.fromList [DotPattern "hs"] -- TODO

apprehend :: Config -> IO ()
apprehend config@Config{..} = do
  putLn "attempting apprehension..."
  print config
  build (unLensText lensText) >>= \case
    Left  err  -> panic . show $ err
    Right f ->
      S.mapM_ ( processWith f )
      . S.take 1 -- TODO
      . S.chain (putLn . ("DEBUG FILENAME: " <>) . show . fst)
      . S.filter ((".hs" `List.isSuffixOf`) . fst)
      . S.filter (not . (".stack-work" `List.isInfixOf`) . fst)
      . S.filter (not . isDirectory . snd)
      . tree
      . unTarget
      $ target

processWith :: (ByteString -> ByteString) -> FileInfo -> IO ()
processWith f (path, stat) = putLn $ "would process file here: " <> show path

-- apprehend :: Config -> IO ()
-- apprehend config@Config{..} = do
--   putLn "attempting apprehension..."
--   print config
--   let lensText' :: Text
--       lensText' = unLensText lensText
--   build lensText' >>= \case
--     Left  err  -> panic . show $ err
--     Right trav -> do
--       let _ = trav :: ATraversal' StartingPoint (SrcSpanInfo, Int)
--       S.print
--       . S.mapM ( (viewOrApply trav compiledF =<<) . start )
--       . S.take 1 -- TODO
--       . S.chain (putLn . ("DEBUG FILENAME: " <>) . show . fst)
--       . S.filter ((".hs" `List.isSuffixOf`) . fst)
--       . S.filter (not . (".stack-work" `List.isInfixOf`) . fst)
--       . S.filter (not . isDirectory . snd)
--       . tree
--       . unTarget
--       $ target
--   where
--     start :: FileInfo -> IO StartingPoint
--     start _fileInfo@(path, _) = StartingPoint
--       <$> ( (,)
--             <$> pure path
--             <*> (encodeUtf8 <$> readFile path)
--           )

--     compiledF :: Maybe (a -> b)
--     compiledF = Just $ panic "compiledF undefined"

-- viewOrApply :: Typeable a
--             => ATraversal' StartingPoint (SrcSpanInfo, a)
--             -> Maybe (a -> b)
--             -> StartingPoint
--             -> IO ([(FilePath, [SrcSpanInfo])])
-- viewOrApply _trav _fMay _start = panic "viewOrApply undefined"

-- -- applyTraversal :: ATraversal' StartingPoint b
-- --                -> StartingPoint
-- --                -> StartingPoint
-- -- applyTraversal = panic "applyTraversal undefined"

-- -- saveResultsWhenAppropriate :: StartingPoint -> IO ()
-- -- saveResultsWhenAppropriate = panic "saveResultsWhenAppropriate undefined"

-- -- -withLens :: CommonConfig -> ReplaceConfig -> IO ()
-- -- -withLens CommonConfig {..} ReplaceConfig {..} = do
-- -- -  f <- either (panic . show) identity
-- -- -       <$> compileMapFn (unMapFnText mapFnText) -- TODO: before now, cache, etc.
-- -- -  let previewReplacements :: ATraversal' (Module SrcSpanInfo) SrcSpanInfo
-- -- -                          -> (FilePath, FileStatus)
-- -- -                          -> IO ()
-- -- -      previewReplacements t (p, _) = findMatches f t p
-- -- -        >>= mapM_ (\x -> TempSearch.printPrettily theme x >> newLine)
-- -- -  makeLens (unLensText lensText) >>= \case
-- -- -    Left err -> displayError theme err
-- -- -    Right trav -> S.mapM_ (previewReplacements trav)
-- -- -      . S.chain reportFile
-- -- -      . S.filter (\(p, _) -> ".hs" `L.isSuffixOf` p && not (".stack-work" `L.isInfixOf` p))
-- -- -      . tree
-- -- -      . unTarget
-- -- -      $ target
-- -- -  where
-- -- -    reportFile (p, _) = putChunkLn (chunk (pack p) & filename theme)
