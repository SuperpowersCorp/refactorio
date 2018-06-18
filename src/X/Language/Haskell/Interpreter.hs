{-# LANGUAGE NoImplicitPrelude #-}

module X.Language.Haskell.Interpreter
     ( module Language.Haskell.Interpreter
     , build
     ) where

import Refactorio.Prelude

import Language.Haskell.Interpreter
import System.Directory

build :: Typeable a => Text -> IO (Either InterpreterError a)
build src = getHomeDirectory >>= \home -> runInterpreter $ do
  set [ languageExtensions
        := [ FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , MultiParamTypeClasses
           , LambdaCase
           , OverloadedStrings
           -- , RankNTypes
           , RecordWildCards
           -- , ScopedTypeVariables
           ]
      ]
  loadModules [ home <> "/.refactorio/InterPrelude.hs" ]
  setImports
    [ "Protolude"
    , "Control.Lens"
    , "Data.Data.Lens"
    , "Language.Haskell.Exts"
    , "Language.Haskell.Exts.Prisms"
    , "Refactorio.InterPrelude"
    ]
  interpret (unpack src) infer
