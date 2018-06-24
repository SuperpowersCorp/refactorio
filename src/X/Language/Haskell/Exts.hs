{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module X.Language.Haskell.Exts
     ( module Exports
     , hs
     ) where

import Refactorio.Prelude               hiding ( get )

import Data.ByteString.Char8 as Char8
import Language.Haskell.Exts as Exports

hs :: Iso' ByteString (Module SrcSpanInfo)
hs = iso get set
  where
    get :: ByteString -> Module SrcSpanInfo
    get bs = case parseFileContentsWithMode mode' . Char8.unpack $ bs of
      ParseOk m -> m
      other     -> panic . show $ other

    set :: Module SrcSpanInfo -> ByteString
    set = panic "hs set' undefined"

    mode' :: ParseMode
    mode' = ParseMode "refactorio-live" Haskell2010 defaultExtensions
      False False Nothing False
      where
        defaultExtensions = []
