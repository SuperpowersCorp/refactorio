{-# LANGUAGE NoImplicitPrelude #-}

module Refactorio.Config where

import Refactorio.Prelude

import Refactorio.Theme


data Config = Config
  { projectRoot :: FilePath
  , lensText    :: Text
  , theme       :: Theme
  , mapFn       :: MapFn
  , save        :: SaveMode
  }

data MapFn = FMapFn | NoMapFn | SMapFn

data SaveMode = Backup | Overwrite | View
