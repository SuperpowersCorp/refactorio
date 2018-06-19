{-# LANGUAGE NoImplicitPrelude #-}

module X.Streaming
     ( module Streaming
     , concatStreams
     ) where

import Refactorio.Prelude

import Streaming

concatStreams :: Monad m => [Stream (Of a) m ()] -> Stream (Of a) m ()
concatStreams = foldl f z
  where
    f x y = x >>= const y
    z     = return ()
