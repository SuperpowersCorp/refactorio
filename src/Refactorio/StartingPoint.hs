{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Refactorio.StartingPoint where

import Refactorio.Prelude

newtype StartingPoint = StartingPoint (FilePath, ByteString)
  deriving (Data, Eq, Ord, Read, Show, Typeable)

