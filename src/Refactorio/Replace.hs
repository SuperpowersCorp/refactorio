{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Refactorio.Replace where

import           Refactorio.Prelude
import qualified Streaming.Prelude            as S

import           Control.Lens                 as A          hiding ( (&)
                                                                   , pre
                                                                   , set
                                                                   )
import qualified Data.List                    as L
import           Data.Text
import           Language.Haskell.Exts                      hiding ( OverloadedStrings )
import           Language.Haskell.Interpreter
import           Refactorio.Config
import           Refactorio.Search            as TempSearch
import           Refactorio.Theme
import           X.Rainbow                                  hiding ( (&) )
import           X.Streaming.Files                                 ( tree )

type MapFn = Module SrcSpanInfo -> Module SrcSpanInfo

compileMapFn :: Text -> IO (Either InterpreterError MapFn)
compileMapFn src = runInterpreter $ do
  set [ languageExtensions := [ OverloadedStrings ] ]
  loadModules
    [ "/Users/john/.refactorio/InterPrelude.hs"
    ]
  setImports
    [ "Protolude"
    , "Control.Lens"
    , "Data.Data.Lens"
    , "Language.Haskell.Exts"
    , "Language.Haskell.Exts.Prisms"
    , "Refactorio.InterPrelude"
    ]
  interpret (unpack src) infer

makeLens :: Text -> IO (Either InterpreterError
                        (ATraversal' (Module SrcSpanInfo) SrcSpanInfo))
makeLens s = runInterpreter $ do
  loadModules
    [ "/Users/john/.refactorio/InterPrelude.hs"
    ]
  setImports
    [ "Prelude"
    , "Control.Lens"
    , "Data.Data.Lens"
    , "Language.Haskell.Exts"
    , "Language.Haskell.Exts.Prisms"
    , "Refactorio.InterPrelude"
    ]
  interpret (unpack s) infer

displayError :: InterpreterError -> IO ()
displayError = panic . show  -- TODO

-- TODO: DRY up vs Search.byLens
withLens :: Config -> MapFn -> IO ()
withLens Config {..} f = S.mapM_ (previewReplacements trav)
  . S.chain reportFile
  . S.filter (\(p, _) -> ".hs" `L.isSuffixOf` p && not (".stack-work" `L.isInfixOf` p))
  . tree
  $ projectRoot
  where
    trav = convert f

    reportFile (p, _) = putChunkLn (chunk (pack p) & filename theme)

    -- previewReplacements :: ATraversal' (Module SrcSpanInfo) SrcSpanInfo
    --                     -> (String, FileStatus)
    --                     -> IO a
    previewReplacements t (p, _) = TempSearch.findMatches t p
      >>= mapM_ (\x -> TempSearch.printPrettily theme x >> newLine)

    convert :: MapFn -> ATraversal' (Module SrcSpanInfo) SrcSpanInfo
    convert _= panic "convert undefined"
