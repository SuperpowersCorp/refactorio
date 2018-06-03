{-# LANGUAGE MultiParamTypeClasses #-}

module Refactorio.InterPrelude where

import Control.Lens
import Data.Data.Lens
import Language.Haskell.Exts
import Language.Haskell.Exts.Prisms

end :: (Functor f, Field1 s t a b) => (a -> f b) -> s -> f t
end = _1

srcSpanInfoL :: Lens' SrcSpanInfo SrcSpan
srcSpanInfoL = lens srcInfoSpan $ \ssi sis -> ssi { srcInfoSpan = sis }

moduleNameL :: Applicative f
            => LensLike f (Module SrcSpanInfo) (Module SrcSpanInfo) SrcSpan SrcSpan
moduleNameL = _Module._2._Just._ModuleHead._2._ModuleName._1.srcSpanInfoL

anyModuleNameL :: Applicative f
               => LensLike f (Module SrcSpanInfo) (Module SrcSpanInfo) SrcSpan SrcSpan
anyModuleNameL = _Module . biplate . _ModuleName . _1 . srcSpanInfoL
