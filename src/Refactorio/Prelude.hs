{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Refactorio.Prelude
    ( module Exports
    , newLine
    , putLn
    ) where

import Control.Arrow  as Exports ( (&&&) )
import Control.Lens   as Exports ( (.~)
                                , makeClassy
                                )
import Data.Data.Lens as Exports ( upon )
import Data.Data      as Exports ( Data )
import Data.Text      as Exports ( unpack )
import Protolude      as Exports

putLn :: MonadIO m => Text -> m ()
putLn = putStrLn

newLine :: MonadIO m => m ()
newLine = putLn ""
