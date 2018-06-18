{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Refactorio.DeleteMe where

import Refactorio.Prelude

import Codec.Compression.Zlib.Lens as Z
import Control.Lens                as L
import Data.Aeson.Lens             as A
import Data.String.Conv            as S

foo :: ByteString -> ByteString
foo = L.over ( (S.convS :: Lens' ByteString LByteString) . gzipped . (S.convS :: Lens' LByteString ByteString) . A.key "baz" . A._Number ) (*5)


-- % refio -t /tmp/voltron/src/*.gz -p 'L.over ((S.convS :: ByteString -> LByteString) . gzipped . (S.convS :: LByteString -> Bytestring) . A.key "baz" . A._Number) (*5)'
