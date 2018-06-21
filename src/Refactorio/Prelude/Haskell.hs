{-# LANGUAGE NoImplicitPrelude #-}

module Refactorio.Prelude.Haskell
     ( module Exports
     , end
     ) where

import Refactorio.Prelude.Basic       as Exports hiding ( Alt
                                                        , Context
                                                        , Fixity
                                                        , List
                                                        , SrcLoc
                                                        , _Cons
                                                        , mod
                                                        , op
                                                        , sym
                                                        )

import Language.Haskell.Exts          as Exports
import X.Language.Haskell.Exts.Prisms as Exports

end :: Field1 s t a b => Lens s t a b
end = _1
