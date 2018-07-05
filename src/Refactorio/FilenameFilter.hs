{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Refactorio.FilenameFilter
     ( FilenameFilter(..)
     , compileFilter
     , devMatchesAnyString -- For dev
     , expandExtraFilters
     , matches
     , matchesAny
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

devMatchesAnyString :: [String] -> FilePath -> Bool
devMatchesAnyString = matchesAny . map (compileFilter . FilenameFilter . pack)

expandExtraFilters :: Maybe SpecialMode -> Set FilenameFilter -> Set FilenameFilter
expandExtraFilters specialModeMay existing
  | not . null $ existing = existing
  | otherwise = maybe empty filtersForSpecialMode specialModeMay

filtersForSpecialMode :: SpecialMode -> Set FilenameFilter
filtersForSpecialMode m = fromList . map FilenameFilter $ case m of
  Haskell    -> [ "**/*.hs" ]
  Html       -> [ "**/*.html", "**/*.xhtml", "**/*.htm" ]
  JavaScript -> [ "**/*.js" ]
  Json       -> [ "**/*.json" ]
  Xml        -> [ "**/*.xml" ]
  Yaml       -> [ "**/*.yaml"
                , "**/*.yml"
                ]

matches :: CompiledFilter -> FilePath -> Bool
matches = match

matchesAny :: [CompiledFilter] -> FilePath -> Bool
matchesAny filters path
  | null filters = True
  | otherwise    = any (`matches` path) filters
