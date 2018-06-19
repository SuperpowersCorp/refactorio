{-# LANGUAGE NoImplicitPrelude #-}

module Refactorio.Prelude.Xml
     ( module Exports
     , xmlL
     ) where

import Refactorio.Prelude.Basic as Exports hiding ( children
                                                  , element
                                                  , elements
                                                  )

import Data.String.Conv                           ( convS )
import Text.Xml.Lens            as Exports

xmlL :: Lens' ByteString LByteString
xmlL = convS
