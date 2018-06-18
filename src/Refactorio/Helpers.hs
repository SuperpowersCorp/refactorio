{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Refactorio.Helpers
     ( Lazyboy(..)
     , lazy
     , text
     ) where

import Refactorio.Prelude
import Control.Lens (Iso', iso)

class Lazyboy s l where
  strictify :: l -> s
  lazify    :: s -> l

lazy :: Lazyboy s l => Iso' s l
lazy = iso lazify strictify

text :: Iso' ByteString Text
text = iso decodeUtf8 encodeUtf8
