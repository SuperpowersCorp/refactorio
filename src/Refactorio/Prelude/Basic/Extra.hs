{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Refactorio.Prelude.Basic.Extra
     ( Convert(..)
     , a
     , an
     , convertTo
     ) where

import Protolude hiding (from)

import Control.Lens         as L
import Data.Text            as T
import Data.Text.Lazy       as LT
import Data.ByteString      as BS
import Data.ByteString.Lazy as LBS
import Data.Text.Encoding   as TE
import Data.Text.Lens       as TL
import Data.String          as S

class Convert a b where
  convert :: Iso' a b

instance Convert BS.ByteString LBS.ByteString where
  convert = iso LBS.fromStrict LBS.toStrict

instance Convert LBS.ByteString BS.ByteString where
  convert = iso LBS.toStrict LBS.fromStrict

instance Convert T.Text LT.Text where
  convert = iso LT.fromStrict LT.toStrict

instance Convert LT.Text T.Text where
  convert = iso LT.toStrict LT.fromStrict

instance Convert BS.ByteString T.Text where
  convert = iso TE.decodeUtf8 TE.encodeUtf8

instance Convert T.Text BS.ByteString where
  convert = from convert

instance Convert S.String T.Text where
  convert = packed

instance Convert S.String LT.Text where
  convert = packed

instance Convert T.Text S.String where
  convert = unpacked

instance Convert LT.Text S.String where
  convert = unpacked

a :: a
a = panic "witness used inappropriately."

an :: a
an = a

-- | Useful when target type is ambiguous, like so:
--
--     % refio --html -t /tmp/foo '& convertTo(a::LByteString).xml...name %~ Text.toUpper'
--
convertTo :: Convert a b => b -> Iso' a b
convertTo _ = convert
