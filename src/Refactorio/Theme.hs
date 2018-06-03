{-# LANGUAGE NoImplicitPrelude #-}

module Refactorio.Theme where

import Rainbow.Extra


data Theme = Theme
  { filename    :: Chunk' -> Chunk'
  , match       :: Chunk' -> Chunk'
  , searchHdr   :: Chunk' -> Chunk'
  , searchValue :: Chunk' -> Chunk'
  , withinHdr   :: Chunk' -> Chunk'
  , withinValue :: Chunk' -> Chunk'
  }

defaultTheme :: Theme
defaultTheme = Theme
  { filename    = \c -> c & fore green
  , match       = \c -> c & fore yellow & inverse
  , searchHdr   = \c -> c & fore red
  , searchValue = \c -> c & fore red & inverse & bold
  , withinHdr   = \c -> c & fore red
  , withinValue = \c -> c & fore red & bold
  }
