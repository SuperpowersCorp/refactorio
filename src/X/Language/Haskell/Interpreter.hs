{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module X.Language.Haskell.Interpreter
     ( module Language.Haskell.Interpreter
     , build
     ) where

import Refactorio.Prelude                  hiding ( get )

import Data.Char                                  ( isSpace )
import Data.List.Split                            ( splitOn )
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe        ( unsafeRunInterpreterWithArgs )
import Text.Printf                                ( printf )
import X.Language.Haskell.TH                      ( lookupCompileEnvExp )

extraGhcArgs :: [String] -> [String]
extraGhcArgs = fmap (printf "-package-db %s")

build :: Typeable a => [String] -> Text -> IO (Either InterpreterError a)
build possiblePreludes src = do
  args <- extraGhcArgs <$> packageDbPaths
  putLn $ "ARGS: " <> show args
  unsafeRunInterpreterWithArgs args $ do
    -- TODO: make extensions CLI options
    set [ languageExtensions
          := [ AllowAmbiguousTypes
             , FlexibleContexts
             , FlexibleInstances
             , GADTs
             , FunctionalDependencies
             , MultiParamTypeClasses
             , NoImplicitPrelude
             , LambdaCase
             , OverloadedStrings
             , QuasiQuotes
             , RecordWildCards
             , ScopedTypeVariables
             , TupleSections
             , TypeApplications
             ]
        ]
    -- TODO: allow setting via CLI / runtime environment variable
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

packageDbPaths :: IO [FilePath]
packageDbPaths = do
  paths <- runtimePackageDbPaths
  return $ case paths of
    [] -> compileTimePackageDbPaths
    other -> other

-- TODO: implement to allow overriding on command line / from env
runtimePackageDbPaths :: IO [FilePath]
runtimePackageDbPaths = return []

-- TODO: handle non-sandbox installs
compileTimePackageDbPaths :: [FilePath]
compileTimePackageDbPaths = case compileTimePathsMay of
  Nothing -> ["compileTimePackageDbPaths-was-nothing"]
  Just s  -> filter (not . null . trim) . splitOn ":" $ s
  where
    compileTimePathsMay = $(lookupCompileEnvExp "HASKELL_PACKAGE_SANDBOXES")

trim :: String -> String
trim = f . f
   where
     f = reverse . dropWhile isSpace
