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
  , searchHdr   = \c -> c & fore brightBlue
  , searchValue = \c -> c & fore blue & inverse
  , withinHdr   = \c -> c & fore brightBlue
  , withinValue = \c -> c & fore blue
  }
