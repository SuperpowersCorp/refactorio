{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Refactorio.Engine ( process ) where

import           Refactorio.Prelude             as P   hiding ( (<>) )
import qualified Streaming.Prelude              as S

import           Data.Algorithm.DiffContext
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as C8
import qualified Data.Set                       as Set
import           Data.Text                                    ( pack
                                                              , toLower
                                                              )
import           Refactorio.FilenameFilter
import           Refactorio.Types
import           System.Directory
import           System.IO                                    ( hFlush
                                                              , stdout
                                                              )
import           System.Posix.Files
import           Text.PrettyPrint               as PP  hiding ( (<>) )
import           X.Language.Haskell.Interpreter               ( build )
import           X.Rainbow
import           X.Streaming.Files                            ( tree )

-- CURRENT TARGET:   refio --haskell view "__Module.biplate._Int" & "(+32)"

process :: Config -> IO ()
process Config{..} = do
  home <- getHomeDirectory
  putLn $ "Target: "  <> show (unTarget target)
  putLn $ "Filters: " <> show (map unFilenameFilter . Set.toList $ allFilters)
  case specialMode of
    Nothing -> return ()
    Just mode -> putLn $ "Special mode activated: " <> show mode
  putLn $ "Expression: " <> show (unExpression expr)
  putLn $ "UpdateMode: " <> show updateMode
  hFlush stdout
  let prelude :: FilePath = fromMaybe (defaultPrelude home)
                            . fmap (prepend home)
                            . join
                            . fmap specialPrelude
                            $ specialMode
  build prelude (unExpression expr) >>= either (panic . show) process'
  where
    process' f = S.mapM_ ( processWith updateMode f )
      . S.filter ( matchesAny compiledFilters )
      . S.filter ( not . ignored )
      . S.map fst
      . S.filter ( not . isDirectory . snd )
      . tree
      . unTarget
      $ target

    prepend :: FilePath -> FilePath -> FilePath
    prepend home = ((home <> "/") <>)

    allFilters = expandExtraFilters filenameFilters

    compiledFilters = map compileFilter . Set.toList $ allFilters

    defaultPrelude home = home <> "./refactorio"

    expandExtraFilters :: Set FilenameFilter -> Set FilenameFilter
    expandExtraFilters existing
      | not . null $ existing = existing
      | otherwise = fromMaybe Set.empty . fmap filtersForSpecialMode $ specialMode

specialPrelude :: SpecialMode -> Maybe FilePath
specialPrelude Haskell = Just "HaskellPrelude.hs"
specialPrelude Json    = Just "JsonPrelude.hs"
specialPrelude Yaml    = Just "YamlPrelude.hs"

filtersForSpecialMode :: SpecialMode -> Set FilenameFilter
filtersForSpecialMode m = Set.fromList . map FilenameFilter $ case m of
  Haskell -> [ "**/*.hs" ]
  Json    -> [ "**/*.json" ]
  Yaml    -> [ "**/*.yaml"
             , "**/*.yml"
             ]

-- TODO: read .*ignore files from the target dir down to the current file, caching
--       along the way, etc. but for now...
ignored :: FilePath -> Bool
ignored path = False
  || path `contains`   ".stack-work"
  || path `startsWith` ".git/"
  || path `contains`   "/.git/"

data ChangeChoice = AcceptChange | RejectChange | QuitChanges
  deriving (Data, Eq, Ord, Read, Show, Typeable)

changePrompt :: IO ChangeChoice
changePrompt = do
  putStr ("Accept change (Y/N/Q)? " :: Text)
  hFlush stdout
  (toLower <$> getLn) >>= \case
    "y"    -> return AcceptChange
    "yes"  -> return AcceptChange
    "n"    -> return RejectChange
    "no"   -> return RejectChange
    "q"    -> return QuitChanges
    "quit" -> return QuitChanges
    _      -> changePrompt

processWith :: UpdateMode -> (ByteString -> ByteString) -> FilePath -> IO ()
processWith updateMode f path = do
  (beforeBytes, afterBytes) <- (identity &&& f) <$> BS.readFile path
  putLn $ "Read bytes: " <> show (BS.length beforeBytes)
  putLn $ "Processed bytes: " <> show (BS.length afterBytes)
  if beforeBytes == afterBytes
    then putLn $ "** Unchanged: " <> pack path
    else handleChange (beforeBytes, afterBytes)
  where
    handleChange (beforeBytes, afterBytes) = do
      putLn "Handling change..."
      let beforeLines = C8.lines beforeBytes
          afterLines  = C8.lines afterBytes
          diff'       = getContextDiff ctxLines beforeLines afterLines
          doc         = prettyContextDiff beforeName afterName elPrint diff'
      putLn "here we go..."
      hFlush stdout
      case updateMode of
        AskMode -> do
          showChanges "Review" doc
          changePrompt >>= \case
            AcceptChange -> saveChanges afterBytes
            RejectChange -> putLn "File unchanged."
            QuitChanges  -> do
              putLn "Exiting at user's request."
              exitSuccess
        ReviewMode -> do
          saveChanges afterBytes
          showChanges "Review" doc
        JustDoItMode -> do
          saveChanges afterBytes
          putLn $ "Changed: " <> show path
        PreviewMode ->
          showChanges "Preview" doc

    showChanges label doc = do
      nl
      putLn $ label <> " of changes to: " <> show path
      putLn "================================================================"
      display doc

    saveChanges :: ByteString -> IO ()
    saveChanges = panic "saveChanges!" -- BS.writeFile path

    beforeName :: Doc
    beforeName = PP.text $ path <> " BEFORE"

    afterName :: Doc
    afterName = PP.text $ path <> " AFTER"

    ctxLines :: Int
    ctxLines = 2

    elPrint :: ByteString -> Doc
    elPrint = PP.text . unpack . decodeUtf8

    render' :: Doc -> Text
    render' = pack . PP.render

    -- TODO: altDisplay that putChunkLn's according to - or + in front
    display :: Doc -> IO ()
    display = putLn . render'
