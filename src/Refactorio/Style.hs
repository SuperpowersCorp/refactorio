{-# LANGUAGE NoImplicitPrelude #-}

module Refactorio.Style where

import Rainbow.Extra


data Style = Style
  { filename    :: Chunk' -> Chunk'
  , match       :: Chunk' -> Chunk'
  , searchHdr   :: Chunk' -> Chunk'
  , searchValue :: Chunk' -> Chunk'
  , withinHdr   :: Chunk' -> Chunk'
  , withinValue :: Chunk' -> Chunk'
  }

defaultStyle :: Style
defaultStyle = Style
  { filename    = \c -> c & fore green
  , match       = \c -> c & fore yellow & inverse
  , searchHdr   = \c -> c & fore brightBlue
  , searchValue = \c -> c & fore blue & inverse
  , withinHdr   = \c -> c & fore brightBlue
  , withinValue = \c -> c & fore blue
  }
