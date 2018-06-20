{-# LANGUAGE NoImplicitPrelude #-}

module Refactorio.Prelude.Basic
     ( module Exports
     ) where

import Control.Lens     as Exports
import Data.Data.Lens   as Exports
import Protolude        as Exports hiding ( (<&>)
                                          , (<.>)
                                          , Strict
                                          , from
                                          , to
                                          , uncons
                                          , unsnoc
                                          )
import Text.Regex.Lens  as Exports
import Text.Regex.PCRE  as Exports        ( Regex )
import Text.Regex.Quote as Exports

