{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module X.Language.Haskell.Interpreter
     ( module Language.Haskell.Interpreter
     , build
     ) where

import Refactorio.Prelude                  hiding ( get )

import Data.String (lines)
import Data.Char                                  ( isSpace )
import Data.List.Split                            ( splitOn )
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe        ( unsafeRunInterpreterWithArgs )
import Text.Printf                                ( printf )
import X.Language.Haskell.TH                      ( lookupCompileEnvExp )
import System.Environment                         ( lookupEnv )
import System.Process                             ( readProcessWithExitCode )

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

runtimePackageDbPaths :: IO [FilePath]
runtimePackageDbPaths = lookupEnv "REFACTORIO_PACKAGE_PATH" >>= \case
  Nothing -> return []
  Just pathStr -> do
    paths <- expand pathStr
    return . filter (not . null . trim) . splitOn ":" $ paths
  where
    expand :: String -> IO String
    expand path
      | path `endsWith` ":" = do
          defaultDatabasePath <- getDefaultPackageDbPath
          putLn $ "DEFAULT DATABASE PATH: " <> show defaultDatabasePath
          return $ path ++ defaultDatabasePath
      | otherwise = return path

-- TODO: must be a better way
getDefaultPackageDbPath :: IO FilePath
getDefaultPackageDbPath = readProcessWithExitCode "ghc-pkg" ["list"] "" >>= \case
  (ExitFailure _, _, stderr') ->
    panic $ "Failed to read default package database path:\n" <> show stderr'
  (ExitSuccess, stdout', _) -> return . parsePathsFromGhcPkgList $ stdout'

parsePathsFromGhcPkgList :: String -> FilePath
parsePathsFromGhcPkgList = intercalate ":" . filter (`startsWith` "/") . lines

-- TODO: handle non-sandbox installs
compileTimePackageDbPaths :: [FilePath]
compileTimePackageDbPaths = case compileTimePathsMay of
  Nothing -> ["compileTimePackageDbPaths-was-nothing"]
  -- TODO: use ';' instead ':' on windows
  Just s  -> filter (not . null . trim) . splitOn ":" $ s
  where
    compileTimePathsMay = $(lookupCompileEnvExp "HASKELL_PACKAGE_SANDBOXES")

trim :: String -> String
trim = f . f
   where
     f = reverse . dropWhile isSpace

-- If GHC_PACKAGE_PATH is set, use that
--

-- address this (from https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html#package-databases):
--
-- However, if GHC_PACKAGE_PATH ends in a separator, the default databases
-- (i.e. the user and global package databases, in that order) are appended to
-- the path. For example, to augment the usual set of packages with a database
-- of your own, you could say (on Unix):
