{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Refactorio.Conversions
     ( Convert(..)
     , a
     , an
     , convertFrom
     , convertTo
     , isoAs
     ) where

import Control.Lens         as L
import Data.ByteString      as BS
import Data.ByteString.Lazy as LBS
import Data.String          as S
import Data.Text            as T
import Data.Text.Encoding   as TE
import Data.Text.Lazy       as LT
import Data.Text.Lens       as TL
import Protolude                   hiding ( from )

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

convertFrom :: Convert a b => a -> Iso' a b
convertFrom _ = convert

isoAs :: a -> Iso' a a
isoAs _ = iso identity identity

class Convert a b where
  convert :: Iso' a b

instance Convert a a where
  convert = iso identity identity

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

instance Convert BS.ByteString LT.Text where
  convert = convertTo (a :: T.Text) . convertTo (a :: LT.Text)

instance Convert BS.ByteString String where
  convert = convertTo (a :: T.Text) . unpacked
