{-# LANGUAGE NoImplicitPrelude #-}

module X.Language.Haskell.Interpreter
     ( module Language.Haskell.Interpreter
     , build
     ) where

import Refactorio.Prelude

import Language.Haskell.Interpreter

build :: Typeable a => Maybe FilePath -> Text -> IO (Either InterpreterError a)
build preludePathMay src = runInterpreter $ do
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
  case preludePathMay of
    Just preludePath -> loadModules [ preludePath ]
    Nothing          -> return ()
  setImportsQ
    [ ("Control.Lens"                , Nothing)
    , ("Control.Lens"                , Just "L")
    , ("Data.Aeson.Lens"             , Just "A")
    , ("Data.Data.Lens"              , Just "L")
    , ("Language.Haskell.Exts"       , Just "HS")
    , ("Language.Haskell.Exts.Prisms", Just "HS")
    , ("Protolude"                   , Nothing)
    , ("Text.Pandoc.Lens"            , Just "P")
    , ("Codec.Compression.Zlib.Lens" , Nothing)
    ]
  interpret (unpack src) infer
