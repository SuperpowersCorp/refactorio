{-# LANGUAGE NoImplicitPrelude #-}

module X.Language.Haskell.Interpreter
     ( module Language.Haskell.Interpreter
     , build
     ) where

import Refactorio.Prelude

import Language.Haskell.Interpreter

build :: Typeable a => FilePath -> Text -> IO (Either InterpreterError a)
build preludePath src = runInterpreter $ do
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
  loadModules [ preludePath ]
  setImports
    [ "Protolude"
    , "Control.Lens"
    , "Data.Data.Lens"
    , "Language.Haskell.Exts"
    , "Language.Haskell.Exts.Prisms"
    , "Refactorio.InterPrelude"
    ]
  interpret (unpack src) infer
