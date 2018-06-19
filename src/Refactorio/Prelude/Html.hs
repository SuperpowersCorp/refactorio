{-# LANGUAGE NoImplicitPrelude #-}

module Refactorio.Prelude.Html
     ( module Exports
     , htmlL
     ) where

import Refactorio.Prelude.Basic as Exports hiding ( children
                                                  , element
                                                  , elements
                                                  )

import Data.String.Conv                           ( convS )
import Text.Xml.Lens            as Exports

htmlL :: Lens' ByteString LByteString
htmlL = convS
