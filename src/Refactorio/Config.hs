{-# LANGUAGE NoImplicitPrelude #-}

module Refactorio.Config where

import Refactorio.Prelude


data Config = Config
  { projectRoot :: FilePath
  , lensText    :: Text
  } deriving (Eq, Ord, Read, Show)
