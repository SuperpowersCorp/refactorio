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

import Protolude
import Control.Lens

import Data.Text            as T
import Data.Text.Lazy       as LT
import Data.ByteString      as BS
import Data.ByteString.Lazy as LBS
import Data.Text.Encoding   as TE

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

a :: a
a = panic "witness used inappropriately."

an :: a
an = a

convertTo :: Convert a b => b -> Iso' a b
convertTo _ = convert
