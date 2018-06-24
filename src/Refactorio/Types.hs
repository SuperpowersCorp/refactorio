{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module Refactorio.Types where

import Refactorio.Prelude

import Refactorio.FilenameFilter
import Refactorio.SpecialMode

data ChangeChoice = AcceptChange | RejectChange | QuitChanges
  deriving (Data, Eq, Ord, Read, Show, Typeable)

data Config = Config
  { filenameFilters       :: Set FilenameFilter
  , expr                  :: Expression
  , monadic               :: Bool
  , preludeModuleMay      :: Maybe String
  , specialModeMay        :: Maybe SpecialMode
  , updateMode            :: UpdateMode
  , targets               :: NonEmpty Target
  } deriving (Data, Eq, Ord, Read, Show)

newtype Expression = Expression { unExpression :: Text }
  deriving (Data, Eq, Ord, Read, Show)

data MappingFn
  = MapFn  (ByteString -> ByteString)
  | MapMFn (ByteString -> IO ByteString)

data Result = Result
  deriving (Data, Eq, Ord, Read, Show)

newtype Target = Target { unTarget :: FilePath }
  deriving (Data, Eq, Ord, Read, Show)

data UpdateMode
  = AskMode
  | ModifyMode
  | Nope
  | PreviewMode
  -- | ReplaceMode Text
  | ReviewMode
  | SearchMode
  deriving (Data, Eq, Ord, Read, Show)

data WriteChanges = Yes | No | Ask
  deriving (Data, Eq, Ord, Read, Show)
