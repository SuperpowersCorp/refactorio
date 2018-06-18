{-# LANGUAGE NoImplicitPrelude #-}

module X.Rainbow
     ( module Exports
     , Chunk'
     , putChunks
     , putChunksLn
     ) where

import Refactorio.Prelude

import Rainbow            as Exports

type Chunk' = Chunk Text

putChunks :: Renderable a => [Chunk a] -> IO ()
putChunks = mapM_ putStr . chunksToByteStrings toByteStringsColors256

putChunksLn :: Renderable a => [Chunk a] -> IO ()
putChunksLn cs = putChunks cs >> nl
