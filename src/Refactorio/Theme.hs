{-# LANGUAGE NoImplicitPrelude #-}

module Refactorio.Theme where

import Rainbow.Extra

data Theme = Theme
  { errorColor  :: StyleFn
  , filename    :: StyleFn
  , match       :: StyleFn
  , searchHdr   :: StyleFn
  , searchValue :: StyleFn
  , withinHdr   :: StyleFn
  , withinValue :: StyleFn
  }

type StyleFn = Chunk' -> Chunk'

defaultTheme :: Theme
defaultTheme = Theme
  { errorColor  = \c -> c & fore red
  , filename    = \c -> c & fore green
  , match       = \c -> c & fore yellow & inverse
  , searchHdr   = \c -> c & fore blue
  , searchValue = \c -> c & fore blue & inverse & bold
  , withinHdr   = \c -> c & fore blue
  , withinValue = \c -> c & fore blue & bold
  }
