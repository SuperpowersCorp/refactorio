{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module X.Language.Haskell.TH
     ( lookupCompileEnvExp
     ) where

import Refactorio.Prelude         hiding ( lift )

import Data.List                         ( lookup )
import Language.Haskell.TH
import Language.Haskell.TH.Syntax        ( Lift(..) )
import System.Environment                ( getEnvironment )

-- via https://stackoverflow.com/questions/19679024/how-to-properly-communicate-compile-time-information-to-template-haskell-functio

-- | Looks up a compile-time environment variable. The result is a TH
-- expression of type @Maybe String@.
lookupCompileEnvExp :: String -> Q Exp
lookupCompileEnvExp = ((`sigE` [t| Maybe String |]) . lift) <=< lookupCompileEnv
  -- We need to explicly type the result so that things like `print Nothing`
  -- work.

-- | Looks up a compile-time environment variable.
lookupCompileEnv :: String -> Q (Maybe String)
lookupCompileEnv key = lookup key `liftM` runIO getEnvironment
