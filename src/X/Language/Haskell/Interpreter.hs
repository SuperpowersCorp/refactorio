{-# LANGUAGE TupleSections #-}
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
           -- , RankNTypes
           , QuasiQuotes
           , RecordWildCards
           , ScopedTypeVariables
           ]
      ]
  -- TODO: catch errors and try the rest.
  case head possiblePreludes of
    Just prelude -> loadModules [ prelude ]
    Nothing      -> return ()
  setImportsQ
    [ ("Control.Lens"                , Nothing)
    , ("Control.Lens"                , Just "L")
    , ("Data.Char"                   , Just "Char")
    , ("Data.String"                 , Just "String")
    , ("Data.Text"                   , Just "Text")
    , ("Data.Aeson.Lens"             , Just "J")
    , ("Data.ByteString.Lens"        , Nothing)
    , ("Data.Data.Lens"              , Nothing)
    , ("Data.String.Conv"            , Just "S")
    , ("Language.Haskell.Exts"       , Just "HS")
    , ("Language.Haskell.Exts.Prisms", Just "HS")
    , ("Refactorio.Helpers"          , Just "H")
    , ("Refactorio.Prelude"          , Nothing)
    , ("Text.Pandoc.Lens"            , Just "P")
    , ("Text.Pandoc.Lens"            , Just "Pandoc")
    , ("Text.Regex.Lens"             , Just "R")
    , ("Text.Regex.Lens"             , Just "Regex")
    , ("Text.Regex.Quote"            , Nothing)
    , ("Text.Taggy"                  , Just "Html")
    , ("Text.Taggy"                  , Just "H")
    , ("Text.Taggy.Lens"             , Just "Html")
    , ("Text.Taggy.Lens"             , Just "H")
    , ("Codec.Compression.Zlib.Lens" , Nothing)
    , ("Codec.Compression.Zlib.Lens" , Just "Z")
    ]
  interpret (unpack ("(" <> src <> ")")) infer
