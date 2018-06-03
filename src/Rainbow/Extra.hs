{-# LANGUAGE NoImplicitPrelude #-}

module Rainbow.Extra
     ( module Exports
     , putChunks
     ) where

import Refactorio.Prelude

import Rainbow            as Exports

putChunks :: Renderable a => [Chunk a] -> IO ()
putChunks = mapM_ putStr . chunksToByteStrings toByteStringsColors256
