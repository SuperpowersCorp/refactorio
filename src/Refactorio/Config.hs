{-# LANGUAGE NoImplicitPrelude #-}

module Refactorio.Config where

import Refactorio.Prelude

import Refactorio.Style


data Config = Config
  { projectRoot :: FilePath
  , lensText    :: Text
  , style       :: Style
  }
