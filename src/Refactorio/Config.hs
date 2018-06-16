{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module Refactorio.Config where

import Refactorio.Prelude

import Data.Data          ( Data )
import Refactorio.Theme

data Config
  = ConfigExecute ReplaceConfig
  | ConfigPreview ReplaceConfig
  | ConfigView    SearchConfig
  deriving (Data, Eq, Ord, Read, Show)

-- Not today, lexicography, not today!
data CommonConfig = CommonConfig
  { filenameFilters :: Set FilenameFilter
  , target          :: Target
  , theme           :: Theme
  }

data FilenameFilter
  = DotPattern Text
  | ExplicitFilenameFilter Text
  | Haskell
  | JSON
  | YAML
  deriving (Data, Eq, Ord, Read, Show)

newtype LensText = LensText { unLensText :: Text }
  deriving (Data, Eq, Ord, Read, Show)

newtype MapFnText = MapFnText { unMapFnText :: Text }
  deriving (Data, Eq, Ord, Read, Show)

data ReplaceConfig = ReplaceConfig
  { lensText     :: LensText
  , lensOperator :: LensOperator
  , mapFnText    :: MapFnText
  } deriving (Data, Eq, Ord, Read, Show)

data LensOperator  -- TODO: the rest
  = Over  -- aka (%~)
  | Plus  -- aka (+~)
  | Set   -- aka (.~)
  | Times -- aka (*~)
  | View  -- aka (^.)
  deriving (Data, Eq, Ord, Read, Show)

data SearchConfig = SearchConfig -- YES data, not newtype shut up !
  { primaryLensText :: LensText
  } deriving (Data, Eq, Ord, Read, Show)

newtype Target = Target { unTarget :: FilePath }
  deriving (Data, Eq, Ord, Read, Show)
