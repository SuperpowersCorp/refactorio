{-# LANGUAGE NoImplicitPrelude #-}

module Refactorio.Prelude.Docx
     ( module Exports
     , markdownL
     ) where

import           Refactorio.Prelude.Pandoc as Exports

import qualified Data.ByteString.Lazy      as LBS
import           System.IO.Unsafe                     ( unsafePerformIO )

markdownL :: Prism' ByteString Pandoc
markdownL = prism g s
  where
    g :: Pandoc -> ByteString
    g = LBS.toStrict . unsafePerformIO . writeDocx writerOpts

    s :: ByteString -> Either ByteString Pandoc
    s x = bimap (const x) fst . readMarkdown readerOpts . decodeUtf8 $ x
    -- TODO: expose the media bag then rewrite this in terms of that.

    readerOpts :: ReaderOptions
    readerOpts = def

    writerOpts :: WriterOptions
    writerOpts = def
