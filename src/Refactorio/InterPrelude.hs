{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Refactorio.InterPrelude
     ( module Exports
     , Hackable
     , anyModuleNameL
     , hack
     , hackable
     , moduleNameL
     , srcSpanInfoL
     , target
     ) where

import Refactorio.Prelude

import Control.Lens                 as Exports hiding ( Context
                                                      , List
                                                      , _Cons
                                                      , op
                                                      )
import Data.Data.Lens               as Exports
import Language.Haskell.Exts        as Exports
import Language.Haskell.Exts.Prisms as Exports
import Refactorio.StartingPoint     as Exports

-- end :: (Functor f, Field1 s t a b) => (a -> f b) -> s -> f t
-- end = _1

anyModuleNameL :: Applicative f
               => LensLike f (Module SrcSpanInfo) (Module SrcSpanInfo) SrcSpan SrcSpan
anyModuleNameL = _Module . biplate . _ModuleName . _1 . srcSpanInfoL

class Hackable a b where
  hack :: a -> (SrcSpanInfo, b)

hackable :: Hackable a b => Lens' a (SrcSpanInfo, b)
hackable = panic "hackable undefined"

moduleNameL :: Applicative f
            => LensLike f (Module SrcSpanInfo) (Module SrcSpanInfo) SrcSpan SrcSpan
moduleNameL = _Module._2._Just._ModuleHead._2._ModuleName._1.srcSpanInfoL

srcSpanInfoL :: Lens' SrcSpanInfo SrcSpan
srcSpanInfoL = lens srcInfoSpan $ \ssi sis -> ssi { srcInfoSpan = sis }

target :: (Functor f, Field2 s t a b) => (a -> f b) -> s -> f t
target = _2
