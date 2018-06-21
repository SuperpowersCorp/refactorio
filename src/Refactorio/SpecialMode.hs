{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module Refactorio.SpecialMode where

import Refactorio.Prelude


data SpecialMode
  = Examples
  | Haskell
  | Html
  | Json
  | Xml
  | Yaml
  deriving (Data, Eq, Ord, Read, Show, Typeable)
