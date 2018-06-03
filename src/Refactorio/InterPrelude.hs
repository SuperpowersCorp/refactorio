{-# LANGUAGE MultiParamTypeClasses #-}

module Refactorio.InterPrelude where

import Control.Lens
import Data.Data.Lens
import Language.Haskell.Exts
import Language.Haskell.Exts.Prisms

anyModuleNameL :: Traversal' (Module SrcSpanInfo) SrcSpan
anyModuleNameL = _Module . biplate . _ModuleName . _1 . srcSpanInfoL

end :: (Functor f, Field1 s t a b) => (a -> f b) -> s -> f t
end = _1

moduleNameL :: Traversal' (Module SrcSpanInfo) SrcSpan
moduleNameL = _Module._2._Just._ModuleHead._2._ModuleName._1.srcSpanInfoL

srcSpanInfoL :: Lens' SrcSpanInfo SrcSpan
srcSpanInfoL = lens srcInfoSpan $ \ssi sis -> ssi { srcInfoSpan = sis }

integersL :: Traversal' (Module SrcSpanInfo) Int
integersL = _Module.biplate._Int.end

-- integersL :: Applicative f
--           => LensLike f (Module SrcSpanInfo) (Module SrcSpanInfo) Int Int
-- integersL = _Module.biplate._Int.end

foo :: Traversal' (Module SrcSpanInfo) String
foo = _Module.biplate._String.end

stringsL :: Traversal' (Module SrcSpanInfo) String
stringsL = _Module.biplate._String.end

-- TODO: Why doesn't this work?
-- rationalsL :: Traversal' (Module SrcSpanInfo) Rational
-- rationalsL = _Module.biplate._Frac.end

deleteMe = 5.3
