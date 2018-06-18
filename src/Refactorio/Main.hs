{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Refactorio.Main ( main ) where

import           Refactorio.Prelude    as P    hiding ( (<>) )
import qualified Streaming.Prelude     as S

import qualified Data.ByteString       as BS
import qualified Data.List             as List
import qualified Data.Set              as Set
import qualified Data.Text             as Text
import           Options.Applicative           hiding ( prefs )
import           Refactorio.FilenameFilter
import           Refactorio.Types
import           System.Posix.Files
import           X.Language.Haskell.Interpreter       ( build )
import           X.Rainbow
import           X.Streaming.Files                    ( FileInfo
                                                      , tree
                                                      )

-- CURRENT TARGET:   refio --haskell view "__Module.biplate._Int" & "(+32)"

main :: IO ()
main = void $ customExecParser prefs opts >>= apprehend
  where
    prefs = defaultPrefs
      { prefShowHelpOnError = True
      , prefShowHelpOnEmpty = True
      }

    opts = info (parser <**> helper) $ fullDesc
           <> header   "Refactorio - Optical Refactoring Tool"
           <> progDesc "Zen and the art of optical file maintenance."

parser :: Parser Config
parser = prefixConfigParser
  where
    prefixConfigParser :: Parser Config
    prefixConfigParser = Config
      <$> filenameFilterSetParser
      <*> expressionParser
      <*> previewParser
      <*> targetParser

    _ = Config :: Set FilenameFilter
               -> Expression
               -> PreviewMode
               -> Target
               -> Config

    _fromInfix :: Set FilenameFilter
               -> PreviewMode
               -> Expression
               -> Target
               -> Config
    _fromInfix filts = flip (Config filts)

previewParser :: Parser PreviewMode
previewParser = pure PreviewModeEnabled

expressionParser :: Parser Expression
expressionParser = Expression . Text.pack <$> argument str
  ( metavar "EXPR"
 <> help    "ByteString -> ByteString"
  )

targetParser :: Parser Target
targetParser = Target <$> strOption
  ( long        "target"
 <> short       't'
 <> metavar     "TARGET"
 <> help        "a file/directory to search/replace"
 <> showDefault
 <> value       "/tmp/voltron" -- TODO
  )

filenameFilterSetParser :: Parser (Set FilenameFilter)
filenameFilterSetParser = pure $ Set.fromList [DotPattern "hs"] -- TODO

apprehend :: Config -> IO ()
apprehend Config{..} = build (unExpression expr) >>= either (panic . show) process
  where
    process f = S.mapM_ ( processWith f )
                . S.chain (putLn . ("DEBUG FILENAME: " <>) . show . fst)
                . S.filter (matchesFilters filenameFilters)
                . S.filter (not . ignored)
                . S.filter (not . isDirectory . snd)
                . tree
                . unTarget
                $ target

matchesFilters :: Set FilenameFilter -> FileInfo -> Bool
matchesFilters filters fileInfo
  | Set.null filters = True
  | otherwise        = any (flip matches . fst $ fileInfo) filters

-- TODO: read .*ignore files from the target dir down to the current file, caching
-- along the way, etc. but for now...
ignored :: FileInfo -> Bool
ignored = (".stack-work" `List.isInfixOf`) . fst

processWith :: (ByteString -> ByteString) -> FileInfo -> IO ()
processWith f (path, stat) = when (not . isDirectory $ stat) $ do
  bytes :: ByteString <- BS.readFile path
  let bytes' :: ByteString = f bytes
  if (bytes /= bytes')
    then putLn $ "Changes in " <> show path
    else putLn $ "NO Changes in " <> show path
