{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Refactorio.Helpers
     ( Lazyboy(..)
     , debugL
     , hs
     , lazy
     , text
     , yaml
     ) where

import Refactorio.Prelude
import Control.Lens as L            ( Iso'
                               , iso
                               , from
                               )
import X.Language.Haskell.Exts ( hs )
import qualified Data.Aeson as Json
import qualified Data.Yaml  as Yaml
import qualified Data.ByteString.Lazy as LBS

class Lazyboy s l where
  strictify :: l -> s
  lazify    :: s -> l

instance Lazyboy ByteString LByteString where
  strictify = LBS.toStrict
  lazify    = LBS.fromStrict

lazy :: Lazyboy s l => Iso' s l
lazy = iso lazify strictify

text :: Iso' ByteString Text
text = iso decodeUtf8 encodeUtf8

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
    g = view (L.from lazy) . Json.encode . yamlBsToValue

    yamlBsToValue :: ByteString -> Json.Value
    yamlBsToValue = either (panic "yamlBsToValue decode failed") identity
      . Yaml.decodeEither

    s :: ByteString -> ByteString
    s = (Yaml.encode :: Maybe Json.Value -> ByteString)
      . (Json.decode :: LByteString -> Maybe Json.Value)
      . view lazy
