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
import           Data.Text                                    ( lines
                                                              , pack
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

-- CURRENT TARGET: refio --haskell '& __Module.biplate._Int +~ 32'

process :: Config -> IO ()
process Config{..} = do
  _home <- getHomeDirectory
  case specialModeMay of
    Nothing   -> return ()
    Just mode -> putLn $ "Special processing activated: " <> show mode
  putLn $ "Targets: " <> show (unTarget target)
  unless (null allFilters) $
    putLn $ "Filters: " <> show (map unFilenameFilter . Set.toList $ allFilters)
  putLn $ "Expression: " <> show (unExpression expr)
  hFlush stdout
  -- let interlude :: FilePath = fromMaybe (defaultInterlude home)
  --                           . fmap (prepend home)
  --                           . join
  --                           . fmap specialInterlude
  --                           $ specialMode
  build Nothing (unExpression expr) >>= either (panic . show) process'
  where
    process' f = S.mapM_ ( processWith updateMode f )
      . S.filter ( matchesAny compiledFilters )
      . S.filter ( not . ignored )
      . S.map fst
      . S.filter ( not . isDirectory . snd )
      . tree
      . unTarget
      $ target

    _prepend :: FilePath -> FilePath -> FilePath
    _prepend home = ((home <> "/.refactorio/") <>)

    allFilters = expandExtraFilters filenameFilters

    compiledFilters = map compileFilter . Set.toList $ allFilters

    -- defaultInterlude home = prepend home "Interlude.hs"

    expandExtraFilters :: Set FilenameFilter -> Set FilenameFilter
    expandExtraFilters existing
      | not . null $ existing = existing
      | otherwise = maybe Set.empty filtersForSpecialMode specialModeMay

-- specialInterlude :: SpecialMode -> Maybe FilePath
-- specialInterlude Haskell = Just "HaskellInterlude.hs"
-- specialInterlude Json    = Just "JsonInterlude.hs"
-- specialInterlude Yaml    = Just "YamlInterlude.hs"

filtersForSpecialMode :: SpecialMode -> Set FilenameFilter
filtersForSpecialMode m = Set.fromList . map FilenameFilter $ case m of
  Haskell -> [ "**/*.hs" ]
  Json    -> [ "**/*.json" ]
  Xml     -> [ "**/*.xml" ]
  Yaml    -> [ "**/*.yaml"
             , "**/*.yml"
             ]

-- TODO: read .*ignore files from the target dir down to the current file, caching
--       along the way, etc. but for now...
ignored :: FilePath -> Bool
ignored path = ocd
  || path `contains`   ".stack-work"
  || path `startsWith` ".git/"
  || path `contains`   "/.git/"
  where
    ocd = False

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
  if beforeBytes == afterBytes
    then putLn $ "** Unchanged: " <> pack path
    else handleChange (beforeBytes, afterBytes)
  where
    handleChange (beforeBytes, afterBytes) = do
      let beforeLines = C8.lines beforeBytes
          afterLines  = C8.lines afterBytes
          diff'       = getContextDiff ctxLines beforeLines afterLines
          doc         = prettyContextDiff beforeName afterName elPrint diff'
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
        ModifyMode -> do
          saveChanges afterBytes
          putLn $ "Changed: " <> show path
        PreviewMode ->
          showChanges "Preview" doc

    showChanges :: Text -> Doc -> IO ()
    showChanges label doc = do
      nl
      putChunkLn $ (chunk . unpack $ label <> " of changes to: " <> show path) & fore yellow
      putChunkLn $ chunk divider & fore yellow
      colorDisplay doc
      where
        divider = pack . replicate 64 $ '='

    saveChanges :: ByteString -> IO ()
    saveChanges bs = do
      putLn $ "Saving changes to " <> pack path
      BS.writeFile path bs

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

    colorDisplay :: Doc -> IO ()
    colorDisplay = mapM_ colorLn . lines . render'
      where
        colorLn :: Text -> IO ()
        colorLn s
          | s `startsWith` "-" = putChunkLn $ chunk s & fore red
          | s `startsWith` "+" = putChunkLn $ chunk s & fore green
          | otherwise          = putChunkLn $ chunk s & fore grey
