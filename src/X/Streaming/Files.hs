{-# LANGUAGE NoImplicitPrelude #-}

module X.Streaming.Files
     ( FileInfo
     , tree
     ) where

import           Refactorio.Prelude
import qualified Streaming.Prelude  as S

import           Streaming
import           System.Directory
import           System.Posix.Files

type FileInfo = (FilePath, FileStatus)

tree :: MonadIO m => FilePath -> Stream (Of FileInfo) m ()
tree path = do
  pathStat <- liftIO $ getFileStatus path
  let selfStream = S.yield (path, pathStat)
  if not (isDirectory pathStat)
    then selfStream
    else do
      children <- concatStreams . map (tree . ((path ++ "/") ++)) . sort
                    <$> liftIO (listDirectory path)
      selfStream >>= const children

concatStreams :: Monad m => [Stream (Of a) m ()] -> Stream (Of a) m ()
concatStreams = foldl f z
  where
    f x y = x >>= const y
    z     = return ()
