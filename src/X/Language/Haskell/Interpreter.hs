{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module X.Language.Haskell.Interpreter
     ( module Language.Haskell.Interpreter
     , build
     ) where

import Refactorio.Prelude

import Language.Haskell.Interpreter

build :: Typeable a => [String] -> Text -> IO (Either InterpreterError a)
build possiblePreludes src = runInterpreter $ do
  -- TODO: make extensions CLI options
  set [ languageExtensions
        := [ FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , MultiParamTypeClasses
           , NoImplicitPrelude
           , LambdaCase
           , OverloadedStrings
           , QuasiQuotes
           , RecordWildCards
           , ScopedTypeVariables
           ]
      ]
  -- TODO: catch errors and try the rest.
  let preludeImport = maybe [] importPrelude . head $ possiblePreludes
  setImportsQ $
    [ ("Codec.Compression.Zlib.Lens" , Just "Z")
    , ("Control.Lens"                , Just "L")
    , ("Data.Char"                   , Just "Char")
    , ("Data.String"                 , Just "String")
    , ("Data.List.Lens"              , Just "L")
    , ("Data.List.Lens"              , Just "ListL")
    , ("Data.Text"                   , Just "Text")
    , ("Data.Text"                   , Just "T")
    , ("Data.Text.Lens"              , Just "Text")
    , ("Data.Text.Lens"              , Just "T")
    , ("Data.Aeson.Lens"             , Just "J")
    , ("Data.ByteString"             , Just "BS")
    , ("Data.ByteString.Lazy"        , Just "LBS")
    , ("Data.String.Conv"            , Just "S")
    , ("Language.Haskell.Exts"       , Just "HS")
    , ("Language.Haskell.Exts.Prisms", Just "HS")
    , ("Prelude"                     , Just "Prelude")
    , ("Refactorio.Helpers"          , Just "H")
    , ("Refactorio.Helpers"          , Just "Helpers")
    , ("Refactorio.Prelude"          , Just "RP")
    , ("Safe"                        , Just "Safe")
    , ("Text.Pandoc.Lens"            , Just "P")
    , ("Text.Pandoc.Lens"            , Just "Pandoc")
    , ("Text.Regex.Lens"             , Just "R")
    , ("Text.Regex.Lens"             , Just "Regex")
    , ("Text.Regex.Quote"            , Nothing)       -- The only unqualified exception
    , ("Text.Xml.Lens"               , Just "Html")
    , ("Text.Xml.Lens"               , Just "H")
    , ("Text.Xml.Lens"               , Just "Xml")
    , ("Text.Xml.Lens"               , Just "X")
    ] ++ preludeImport
  interpret (unpack ("(" <> src <> ")")) infer

importPrelude :: String -> [(String, Maybe String)]
importPrelude s = [(s, Nothing)]
