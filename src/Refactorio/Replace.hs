{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Refactorio.Replace where

import Refactorio.Prelude

import Language.Haskell.Exts
import Refactorio.Config

-- TODO: others
data MapFn f
  = UnitMapFn (f SrcSpanInfo -> f SrcSpanInfo)

type ReplaceError = Text

compileMapFn :: Text -> Either ReplaceError (MapFn f)
compileMapFn _src = Left "compileMapFn not implemented yet."

displayError :: ReplaceError -> IO ()
displayError = panic . show  -- TODO

withLens :: Config -> MapFn f -> IO ()
withLens _config _f = panic "replaceWith undefined"
