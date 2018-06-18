{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module Refactorio.Types where

import Refactorio.Prelude

import Data.Data                 ( Data )
import Refactorio.FilenameFilter

data Config = Config
  { filenameFilters :: Set FilenameFilter
  , expr            :: Expression
  , specialMode     :: Maybe SpecialMode
  , updateMode      :: UpdateMode
  , target          :: Target
  } deriving (Data, Eq, Ord, Read, Show)

newtype Expression = Expression { unExpression :: Text }
  deriving (Data, Eq, Ord, Read, Show)

data SpecialMode
  = Haskell
  | JSON
  | YAML
  deriving (Data, Eq, Ord, Read, Show, Typeable)

data UpdateMode
  = AskMode
  | PreviewMode
  | ReviewMode
  | JustDoItMode
  deriving (Data, Eq, Ord, Read, Show)

newtype Target = Target { unTarget :: FilePath }
  deriving (Data, Eq, Ord, Read, Show)
