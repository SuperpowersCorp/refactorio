{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Refactorio.Prelude
    ( module Exports
    , Ended
    , endsWith
    , newLine
    , putLn
    ) where

import Control.Arrow  as Exports ( (&&&) )
import Control.Lens   as Exports ( (.~)
                                 , makeClassy
                                 )
import Data.Data      as Exports ( Data )
import Data.Data.Lens as Exports ( upon )
import Data.List      as List
import Data.String    as Exports ( String )
import Data.Text      as Exports ( unpack )
import Protolude      as Exports

class Ended a where
  endsWith :: a -> a -> Bool

instance Eq a => Ended [a] where
  endsWith = flip List.isSuffixOf

putLn :: MonadIO m => Text -> m ()
putLn = putStrLn

newLine :: MonadIO m => m ()
newLine = putLn ""

