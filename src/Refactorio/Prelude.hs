{-# LANGUAGE NoImplicitPrelude #-}

module Refactorio.Prelude
    ( module Exports
    , putLn
    ) where

import Protolude as Exports

putLn :: MonadIO m => Text -> m ()
putLn = putStrLn
