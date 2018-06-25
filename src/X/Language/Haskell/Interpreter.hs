{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module X.Language.Haskell.Interpreter
     ( module Language.Haskell.Interpreter
     , build
     ) where

import Refactorio.Prelude           hiding ( get )

import Language.Haskell.Interpreter
import System.Directory                    ( getHomeDirectory )

build :: Typeable a => [String] -> Text -> IO (Either InterpreterError a)
build possiblePreludes src = getHomeDirectory >>= \home -> runInterpreter $ do
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
  -- TODO: allow setting via CLI
  -- TODO: should we include '.'? probably not?
  set [ searchPath := [ home <> "/src/refactorio/.stack-work/install/x86_64-osx/lts-9.21/8.0.2/pkgdb" ] ]
  -- TODO: catch errors and try the rest.
  setImportsQ hardcodedImports
  interpret (unpack ("(" <> src <> ")")) infer
  where
    preludeImport = maybe [] importPrelude . head $ possiblePreludes

    hardcodedImports =
      [ ("Codec.Compression.Zlib.Lens" , Just "Z")
      , ("Control.Lens"                , Just "L")
      , ("Data.Char"                   , Just "Char")
      , ("Data.List.Lens"              , Just "L")
      , ("Data.List.Lens"              , Just "ListL")
      , ("Data.String"                 , Just "S")
      , ("Data.String"                 , Just "String")
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
      , ("Text.Taggy.Lens"             , Just "Html")
      , ("Text.Taggy.Lens"             , Just "H")
      , ("Text.Xml.Lens"               , Just "Xml")
      , ("Text.Xml.Lens"               , Just "XC")
      , ("Text.XML.Lens"               , Just "XML")
      , ("Text.XML.Lens"               , Just "X")
      , ("Text.XML.Hexml.Lens"         , Just "HX")
      , ("Text.XML.Hexml.Lens"         , Just "Hexml")
      , ("Text.XML.Expat.Lens"         , Just "Expat")
      , ("Text.XML.Expat.Lens"         , Just "EX")
      ] ++ preludeImport

importPrelude :: String -> [(String, Maybe String)]
importPrelude s = [(s, Nothing)]
