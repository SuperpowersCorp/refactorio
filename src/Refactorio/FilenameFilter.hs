{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Refactorio.FilenameFilter
     ( FilenameFilter(..)
     , compileFilter
     , expandExtraFilters
     , matches
     , matchesAny
     , matchesAnyString -- For dev
     ) where

import Refactorio.Prelude     hiding ( empty
                                     , null
                                     )

import Data.Data                     ( Data )
import Data.List                     ( null )
import Data.Set                      ( empty
                                     , fromList
                                     )
import Data.Text                     ( pack )
import Refactorio.SpecialMode
import System.FilePath.Glob          ( Pattern
                                     , compile
                                     , match
                                     )

newtype FilenameFilter = FilenameFilter { unFilenameFilter :: Text }
  deriving (Data, Eq, Ord, Read, Show, Typeable)

type CompiledFilter = Pattern

compileFilter :: FilenameFilter -> CompiledFilter
compileFilter = compile . unpack . unFilenameFilter

expandExtraFilters :: Maybe SpecialMode -> Set FilenameFilter -> Set FilenameFilter
expandExtraFilters specialModeMay existing
  | not . null $ existing = existing
  | otherwise = maybe empty filtersForSpecialMode specialModeMay

filtersForSpecialMode :: SpecialMode -> Set FilenameFilter
filtersForSpecialMode m = fromList . map FilenameFilter $ case m of
  Examples -> [ "**/*.yaml"
              , "**/*.yml"
              ]
  Haskell  -> [ "**/*.hs" ]
  Html     -> [ "**/*.html", "**/*.xhtml", "**/*.htm" ]
  Json     -> [ "**/*.json" ]
  Xml      -> [ "**/*.xml" ]
  Yaml     -> [ "**/*.yaml"
              , "**/*.yml"
              ]

matches :: CompiledFilter -> FilePath -> Bool
matches = match

matchesAny :: [CompiledFilter] -> FilePath -> Bool
matchesAny filters path
  | null filters = True
  | otherwise    = any (`matches` path) filters

matchesAnyString :: [String] -> FilePath -> Bool
matchesAnyString = matchesAny . map (compileFilter . FilenameFilter . pack)
