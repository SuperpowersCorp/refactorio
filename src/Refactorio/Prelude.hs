{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Refactorio.Prelude
    ( module Exports
    , newLine
    , putLn
    ) where

import Control.Arrow as Exports ( (&&&) )
import Protolude     as Exports

putLn :: MonadIO m => Text -> m ()
putLn = putStrLn

newLine :: MonadIO m => m ()
newLine = putLn ""
