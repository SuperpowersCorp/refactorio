{-# LANGUAGE NoImplicitPrelude #-}

module Refactorio.Prelude.Html
     ( module Exports
     , _Html
     ) where

import Refactorio.Prelude.Basic as Exports hiding ( children
                                                  , element
                                                  , elements
                                                  )

import Text.XML
import Text.XML.Lens            as Exports

_Html :: Prism' ByteString Document
_Html = prism g s
  where
    g :: Document -> ByteString
    g = view convert . renderLBS renderSettings

    s :: ByteString -> Either ByteString Document
    s = f . view (convertTo (a :: LByteString))
      where
        f :: LByteString -> Either ByteString Document
        f = first show . parseLBS parseSettings

    renderSettings :: RenderSettings
    renderSettings = def

    parseSettings :: ParseSettings
    parseSettings = def
