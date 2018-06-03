module Refactorio.InterPrelude where

import Control.Lens
import Language.Haskell.Exts
import Language.Haskell.Exts.Prisms

srcInfoSpanL :: Lens' SrcSpanInfo SrcSpan
srcInfoSpanL = lens srcInfoSpan $ \ssi sis -> ssi { srcInfoSpan = sis }

moduleNameL :: Applicative f
            => LensLike f (Module SrcSpanInfo) (Module SrcSpanInfo) SrcSpan SrcSpan
moduleNameL = _Module
  . _2
  . _Just
  . _ModuleHead
  . _2
  . _ModuleName
  . _1
  . srcInfoSpanL

unListL :: Lens' a [a]
unListL = lens pure setL
  where
    setL :: a -> [a] -> a
    setL x []    = x
    setL _ (x:_) = x
