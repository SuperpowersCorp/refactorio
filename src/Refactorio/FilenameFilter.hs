{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module Refactorio.FilenameFilter
     ( FilenameFilter(..)
     , compileFilter
     , matches
     , matchesAny
     , matchesAnyString -- For dev
     ) where

import Refactorio.Prelude   hiding ( null )

import Data.Data                   ( Data )
import Data.List                   ( null )
import Data.Text                   ( pack )
import System.FilePath.Glob        ( Pattern
                                   , compile
                                   , match
                                   )

data FilenameFilter = FilenameFilter { unFilenameFilter :: Text }
  deriving (Data, Eq, Ord, Read, Show, Typeable)

type CompiledFilter = Pattern

compileFilter :: FilenameFilter -> CompiledFilter
compileFilter = compile . unpack . unFilenameFilter

matches :: CompiledFilter -> FilePath -> Bool
matches = match

matchesAny :: [CompiledFilter] -> FilePath -> Bool
matchesAny filters path
  | null filters = True
  | otherwise    = any (`matches` path) filters

matchesAnyString :: [String] -> FilePath -> Bool
matchesAnyString = matchesAny . map (compileFilter . FilenameFilter . pack)
