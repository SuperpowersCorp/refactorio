{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-orphans      #-}

module Refactorio.Helpers
     ( debugL
     , hs
     , yaml
     ) where

import           Refactorio.Prelude

import qualified Data.Aeson              as Json
import qualified Data.Yaml               as Yaml
import           Refactorio.Conversions          ( a
                                                 , convert
                                                 , convertTo
                                                 )
import           Text.Xml.Lens                   ( AsHtmlDocument
                                                 , _HtmlDocument
                                                 )
import           X.Language.Haskell.Exts         ( hs )

instance AsHtmlDocument ByteString where
  _HtmlDocument = convertTo (a :: LByteString) . _HtmlDocument

-- | You can drop this into the middle of a composed lens ala...
--
--     a . b . debugL "hi there " . c . d
--
--   and you will get a log message like "hi there (blah)" where blah is
--   the output of lens `b`.
debugL :: Show a => Text -> Iso' a a
debugL label = iso g s
  where
    g :: Show a => a -> a
    g x = trace msg x
      where
        msg = label <> " " <> show x

    s :: Show a => a -> a
    s x = trace msg x
      where
        msg = label <> " " <> show x

yaml :: Iso' ByteString ByteString
yaml = iso g s
  where
    g :: ByteString -> ByteString
    g = view (from convert) . Json.encode . yamlBsToValue

    yamlBsToValue :: ByteString -> Json.Value
    yamlBsToValue = either (panic "yamlBsToValue decode failed") identity
      . Yaml.decodeEither

    s :: ByteString -> ByteString
    s = (Yaml.encode :: Maybe Json.Value -> ByteString)
      . (Json.decode :: LByteString -> Maybe Json.Value)
      . view convert
