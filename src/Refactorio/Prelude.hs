{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Refactorio.Prelude
    ( module Exports
    , Container(..)
    , Ended(..)
    , Started(..)
    , getLn
    , nl
    , putLn
    ) where

import Control.Arrow  as Exports ( (&&&) )
import Control.Lens   as Exports ( (%~)
                                 , (.~)
                                 , (^.)
                                 , makeClassy
                                 , view
                                 )
import Data.Data      as Exports ( Data )
import Data.Data.Lens as Exports ( upon )
import Data.List      as List
import Data.String    as Exports ( String )
import Data.Text      as Exports ( unpack )
import Data.Text      as Text    ( isPrefixOf )
import Protolude      as Exports

class Container a where
  contains :: a -> a -> Bool

class Ended a where
  endsWith :: a -> a -> Bool

class Started a where
  startsWith :: a -> a -> Bool

instance Eq a => Container [a] where
  contains = flip List.isInfixOf

instance Eq a => Ended [a] where
  endsWith = flip List.isSuffixOf

instance Eq a => Started [a] where
  startsWith = flip List.isPrefixOf

instance Started Text where
  startsWith = flip Text.isPrefixOf

getLn :: MonadIO m => m Text
getLn = liftIO getLine

putLn :: MonadIO m => Text -> m ()
putLn = putStrLn

nl :: MonadIO m => m ()
nl = putLn ""
