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
  putLn $ "DEBUG: preludeImport: " <> show preludeImport
  setImportsQ $
    [ ("Codec.Compression.Zlib.Lens" , Nothing)
    , ("Control.Lens"                , Just "L")
    , ("Data.Char"                   , Just "Char")
    , ("Data.String"                 , Just "String")
    , ("Data.Text"                   , Just "Text")
    , ("Data.Text"                   , Just "T")
    , ("Data.Text.Lens"              , Just "Text")
    , ("Data.Text.Lens"              , Just "T")
    , ("Data.Aeson.Lens"             , Just "J")
    , ("Data.String.Conv"            , Just "S")
    , ("Language.Haskell.Exts"       , Just "HS")
    , ("Language.Haskell.Exts.Prisms", Just "HS")
    , ("Refactorio.Helpers"          , Just "H")
    , ("Refactorio.Helpers"          , Just "Helpers")
    , ("Refactorio.Prelude"          , Just "RP")
    , ("Text.Pandoc.Lens"            , Just "P")
    , ("Text.Pandoc.Lens"            , Just "Pandoc")
    , ("Text.Regex.Lens"             , Just "R")
    , ("Text.Regex.Lens"             , Just "Regex")
    , ("Text.Regex.Quote"            , Nothing)
    , ("Text.Xml.Lens"               , Just "Html")
    , ("Text.Xml.Lens"               , Just "H")
    , ("Text.Xml.Lens"               , Just "Xml")
    , ("Text.Xml.Lens"               , Just "X")
    ] ++ preludeImport
  interpret (unpack ("(" <> src <> ")")) infer

-- We only import qualified stuff now and the only unqualified stuff comes from
-- whatever Prelude is resolved.
--
--   ("Control.Lens"                , Nothing)
-- , ("Data.ByteString.Lens"        , Nothing)
-- , ("Data.Data.Lens"              , Nothing)
-- , ("Refactorio.Prelude"          , Nothing)
-- , ("Text.Regex.Quote"            , Nothing)
-- , ("Codec.Compression.Zlib.Lens" , Nothing)

importPrelude :: String -> [(String, Maybe String)]
importPrelude s = [(s, Nothing)]
