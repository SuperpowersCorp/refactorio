{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Refactorio.Helpers
     ( Lazyboy(..)
     , debugL
     , docx
     , hs
     , lazy
     , text
     , yaml
     ) where

import           Refactorio.Prelude
import System.IO.Unsafe (unsafePerformIO)
import           Control.Lens            as L    ( Iso'
                                                 , from
                                                 , iso
                                                 )
import qualified Data.Aeson              as Json
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Yaml               as Yaml
import           Text.Pandoc                     ( Pandoc
                                                 , readDocx
                                                 , writeDocx
                                                 )
import           Text.Pandoc.Options
import           X.Language.Haskell.Exts         ( hs )
class Lazyboy s l where
  strictify :: l -> s
  lazify    :: s -> l

instance Lazyboy ByteString LByteString where
  strictify = LBS.toStrict
  lazify    = LBS.fromStrict

docx :: Iso' ByteString Pandoc
docx = iso g s
  where
    g :: ByteString -> Pandoc
    g = either (panic . show) fst . readDocx readerOpts . view lazy

    -- TODO: eliminate unsafePerformIO by moving it outside the lens
    s :: Pandoc -> ByteString
    s = LBS.toStrict . unsafePerformIO . writeDocx writerOpts

    readerOpts :: ReaderOptions
    readerOpts = def

    writerOpts :: WriterOptions
    writerOpts = def

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
