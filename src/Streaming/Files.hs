{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Streaming.Files
     ( FileInfo
     , tree
     ) where

import           Refactorio.Prelude

import           Streaming
import qualified Streaming.Prelude as S
import           System.Directory
import           System.Posix.Files

type FileInfo = (FilePath, FileStatus)

tree :: forall m. MonadIO m => FilePath -> Stream (Of FileInfo) m ()
tree path = do
  pathStat <- liftIO $ getFileStatus path
  let selfStream = S.yield (path, pathStat)
  if not (isDirectory pathStat)
    then selfStream
    else do
      children <- concatStreams . map (tree . ((path ++ "/") ++))
                    <$> liftIO (listDirectory path)
      selfStream >>= const children

concatStreams :: Monad m => [Stream (Of FileInfo) m ()] -> Stream (Of FileInfo) m ()
concatStreams = foldl f z
  where
    f x y = x >>= const y
    z     = return ()
