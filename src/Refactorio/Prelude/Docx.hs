{-# LANGUAGE NoImplicitPrelude #-}

module Refactorio.Prelude.Docx
     ( module Exports
     , docxL
     ) where

import           Refactorio.Prelude.Pandoc

import qualified Data.ByteString.Lazy      as LBS
import           System.IO.Unsafe                     ( unsafePerformIO )
import           Text.Pandoc               as Exports
import           Text.Pandoc.Lens          as Exports

docxL :: Prism' ByteString Pandoc
docxL = prism g s
  where
    g :: Pandoc -> ByteString
    g = LBS.toStrict . unsafePerformIO . writeDocx writerOpts

    s :: ByteString -> Either ByteString Pandoc
    s x = bimap (const x) fst . readDocx readerOpts . view lazy $ x
    -- TODO: expose the media bag then rewrite this in terms of that.

    readerOpts :: ReaderOptions
    readerOpts = def

    writerOpts :: WriterOptions
    writerOpts = def
