{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Refactorio.FilenameFilter where

import Refactorio.Prelude

import Data.Char          ( toLower )
import Data.Data          ( Data )

data FilenameFilter
  = DotPattern String
  | Haskell
  | JSON
  | YAML
  deriving (Data, Eq, Ord, Read, Show)

matches :: FilenameFilter -> FilePath -> Bool
matches (DotPattern p) path = map toLower path `endsWith` ('.':map toLower p)
matches Haskell        path = matches (DotPattern "hs")   path
matches JSON           path = matches (DotPattern "json") path
matches YAML           path = matches (DotPattern "yaml") path
                           || matches (DotPattern "yml")  path
