{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Refactorio.Types where

import Refactorio.Prelude

import Data.Data                 ( Data )
import Refactorio.FilenameFilter

data Config = Config
  { filenameFilters :: Set FilenameFilter
  , expr            :: Expression
  , preview         :: PreviewMode
  , target          :: Target
  } deriving (Data, Eq, Ord, Read, Show)

newtype Expression = Expression { unExpression :: Text }
  deriving (Data, Eq, Ord, Read, Show)

data LensOperator  -- TODO: the rest
  = Over
  | Plus
  | Set
  | Times
  | View
  deriving (Data, Eq, Ord, Read, Show)

infixOp :: LensOperator -> Text
infixOp Over  = "%~"
infixOp Plus  = "+~"
infixOp Set   = ".~"
infixOp Times = "*~"
infixOp View  = "^."

newtype LensText = LensText { unLensText :: Text }
  deriving (Data, Eq, Ord, Read, Show, Typeable)

newtype MapFnText = MapFnText { unMapFnText :: Text }
  deriving (Data, Eq, Ord, Read, Show)

data PreviewMode
  = PreviewModeDisabled
  | PreviewModeEnabled
  deriving (Data, Eq, Ord, Read, Show)

data SearchConfig = SearchConfig -- YES data, not newtype shut up !
  { primaryLensText :: LensText
  } deriving (Data, Eq, Ord, Read, Show)

newtype Target = Target { unTarget :: FilePath }
  deriving (Data, Eq, Ord, Read, Show)
