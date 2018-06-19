{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

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
           , NoImplicitPrelude
           , LambdaCase
           , OverloadedStrings
           -- , RankNTypes
           , QuasiQuotes
           , RecordWildCards
           , ScopedTypeVariables
           ]
      ]
  case preludePathMay of
    Just preludePath -> loadModules [ preludePath ]
    Nothing          -> return ()
  setImportsQ
    [ ("Control.Lens"                , Nothing)
    , ("Control.Lens"                , Just "L")
    , ("Data.Char"                   , Just "Char")
    , ("Data.String"                 , Just "String")
    , ("Data.Text"                   , Just "Text")
    , ("Data.Aeson.Lens"             , Just "A")
    , ("Data.ByteString.Lens"        , Nothing)
    , ("Data.Data.Lens"              , Just "L")
    , ("Data.String.Conv"            , Just "S")
    , ("Language.Haskell.Exts"       , Just "HS")
    , ("Language.Haskell.Exts.Prisms", Just "HS")
    , ("Protolude"                   , Nothing)
    , ("Refactorio.Helpers"          , Just "H")
    , ("Refactorio.Prelude"          , Just "RP")
    , ("Text.Pandoc.Lens"            , Just "P")
    , ("Text.Regex.Lens"             , Just "R")
    , ("Text.Regex.Quote"            , Nothing)
    , ("Text.Xml.Lens"               , Just "X")
    , ("Codec.Compression.Zlib.Lens" , Nothing)
    ]
  interpret (unpack ("(" <> src <> ")")) infer
