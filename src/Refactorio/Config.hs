{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Refactorio.Config where

import Refactorio.Prelude

import Data.Data          ( Data )

data Config = Config
  { filenameFilters :: Set FilenameFilter
  , lensOperator    :: LensOperator
  , lensText        :: LensText
  , mapFnTextMay    :: Maybe MapFnText
  , preview         :: PreviewMode
  , target          :: Target
  } deriving (Data, Eq, Ord, Read, Show)

data PreviewMode
  = PreviewModeDisabled
  | PreviewModeEnabled
  deriving (Data, Eq, Ord, Read, Show)

data FilenameFilter
  = DotPattern Text
  | ExplicitFilenameFilter Text
  | Haskell
  | JSON
  | YAML
  deriving (Data, Eq, Ord, Read, Show)

newtype LensText = LensText { unLensText :: Text }
  deriving (Data, Eq, Ord, Read, Show, Typeable)

newtype MapFnText = MapFnText { unMapFnText :: Text }
  deriving (Data, Eq, Ord, Read, Show)

data LensOperator  -- TODO: the rest
  = Over
  | Plus
  | Set
  | Times
  | View
  deriving (Data, Eq, Ord, Read, Show)

-- -- ehhhh?
-- instance Monoid LensOperator where
--   mempty  = View

--   mappend :: LensOperator -> LensOperator -> LensOperator
--   a `mappend` View = a
--   _ `mappend` Set  = Set

--   Over  `mappend` Times = Times
--   Plus  `mappend` Times = Times
--   Set   `mappend` Times = Times
--   Times `mappend` Times = Times
--   View  `mappend` Times = Times

--   Over  `mappend` Plus = Times
--   Plus  `mappend` Plus = Times
--   Set   `mappend` Plus = Times
--   Times `mappend` Plus = Plus   -- to be consistent?
--   View  `mappend` Plus = Times

--   _ `mappend` _ = panic " _ _ _ undefined"

infixOp :: LensOperator -> Text
infixOp Over  = "%~"
infixOp Plus  = "+~"
infixOp Set   = ".~"
infixOp Times = "*~"
infixOp View  = "^."

data SearchConfig = SearchConfig -- YES data, not newtype shut up !
  { primaryLensText :: LensText
  } deriving (Data, Eq, Ord, Read, Show)

newtype Target = Target { unTarget :: FilePath }
  deriving (Data, Eq, Ord, Read, Show)
