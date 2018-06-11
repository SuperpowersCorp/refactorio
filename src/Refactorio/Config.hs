{-# LANGUAGE NoImplicitPrelude #-}

module Refactorio.Config where

import Refactorio.Prelude

import Refactorio.Theme

data Config = Config
  { projectRoot :: FilePath
  , lensText    :: Text
  , theme       :: Theme
  , mapFnSrc    :: Text
  }

-- TODO: soomething like ...
--   , save        :: SaveMode
-- data SaveMode = AtomicSwap | Backup | Overwrite | View
