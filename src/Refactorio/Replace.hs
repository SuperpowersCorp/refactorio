{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Refactorio.Replace where

import           Refactorio.Prelude
import qualified Streaming.Prelude              as S

import           Control.Lens                   as A          hiding ( (&) )
import qualified Data.List                      as L
import           Data.Text
import           Language.Haskell.Exts          as E
import           Language.Haskell.Interpreter
import           Refactorio.Config
import qualified Refactorio.Search              as TempSearch
import           Refactorio.Theme
import           X.Language.Haskell.Interpreter                      ( build )
import           X.Rainbow                                    hiding ( (&) )
import           X.Streaming.Files                                   ( FileStatus
                                                                     , tree
                                                                     )

type MapFn = SrcSpanInfo -> SrcSpanInfo

-- CURRENT TARGET:   refio . --haskell view "__Module.biplate._Int" --pre-mqp "+32"

compileMapFn :: Text -> IO (Either InterpreterError MapFn)
compileMapFn = build

makeLens :: Text -> IO (Either InterpreterError
                        (ATraversal' (Module SrcSpanInfo) SrcSpanInfo))
makeLens = build

displayError :: Theme -> InterpreterError -> IO ()
displayError _theme = panic . show  -- TODO

-- TODO: DRY up vs Search.byLens
withLens :: CommonConfig -> ReplaceConfig -> IO ()
withLens CommonConfig {..} ReplaceConfig {..} = do
  f <- either (panic . show) identity
       <$> compileMapFn (unMapFnText mapFnText) -- TODO: before now, cache, etc.
  let previewReplacements :: ATraversal' (Module SrcSpanInfo) SrcSpanInfo
                          -> (FilePath, FileStatus)
                          -> IO ()
      previewReplacements t (p, _) = findMatches f t p
        >>= mapM_ (\x -> TempSearch.printPrettily theme x >> newLine)
  makeLens (unLensText lensText) >>= \case
    Left err -> displayError theme err
    Right trav -> S.mapM_ (previewReplacements trav)
      . S.chain reportFile
      . S.filter (\(p, _) -> ".hs" `L.isSuffixOf` p && not (".stack-work" `L.isInfixOf` p))
      . tree
      . unTarget
      $ target
  where
    reportFile (p, _) = putChunkLn (chunk (pack p) & filename theme)

findMatches :: MapFn
            -> ATraversal' (Module SrcSpanInfo) SrcSpanInfo
            -> FilePath
            -> IO [SrcSpanInfo]
findMatches f trav path = do
  sourceString <- unpack <$> readFile path
  case parseFileContentsWithMode (parseMode path) sourceString of
    ParseFailed srcLoc' err -> panic $ "ERROR at " <> show srcLoc' <> ": " <> show err
    ParseOk parsedMod       -> return
      $ toListOf (cloneTraversal trav)
                 (parsedMod & cloneTraversal trav %~ f)

-- TODO: finish/DRY up vs Search
parseMode :: FilePath -> ParseMode
parseMode path = defaultParseMode
  { baseLanguage          = Haskell2010
  , ignoreLanguagePragmas = False
  , extensions            = configuredExtensions
  , parseFilename         = path
  }
  where
    configuredExtensions = extensions defaultParseMode ++ tempManualExtensions
    tempManualExtensions = fmap EnableExtension
      [ E.OverloadedStrings
      , E.RankNTypes
      , E.FlexibleContexts
      ]

execute :: CommonConfig -> ReplaceConfig -> IO ()
execute = panic "ReplaceConfig.execute undefined -- YOU ARE NOT READY UNTIL PREVIEW IS IMPLEMENTED"

preview :: CommonConfig -> ReplaceConfig -> IO ()
preview = withLens
