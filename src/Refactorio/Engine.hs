{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Refactorio.Engine ( process ) where

import           Refactorio.Prelude             as P      hiding ( (<>) )
import qualified Streaming.Prelude              as S

import           Data.Algorithm.DiffContext
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as C8
import qualified Data.Set                       as Set
import           Data.Text                                       ( lines
                                                                 , pack
                                                                 , toLower
                                                                 )
import           Refactorio.FilenameFilter
import qualified Refactorio.Legacy              as Legacy
import           Refactorio.SpecialMode
import           Refactorio.Types
import           System.IO                                       ( hFlush
                                                                 , stdout
                                                                 )
import           System.Posix.Files
import           Text.PrettyPrint               as PP     hiding ( (<>) )
import           X.Language.Haskell.Interpreter                  ( GhcError( errMsg )
                                                                 , InterpreterError(..)
                                                                 , build
                                                                 )
import           X.Rainbow
import           X.Streaming.Files                               ( tree )

process :: Config -> IO ()
process config@Config{..} = do
  case updateMode of
      -- ReplaceMode _ -> putLn "Legacy Replace Mode Activated."
      SearchMode    -> putLn "Legacy Search Mode Activated."
      _             -> return ()
  case specialModeMay of
    Nothing   -> return ()
    Just mode -> putLnMay $ "Special processing activated: " <> show mode
  putLnMay $ "Targets: " <> show (unTarget target)
  unless (null allFilters) $
    putLnMay $ "Filters: " <> show (map unFilenameFilter . Set.toList $ allFilters)
  let preferedPreludes :: [String]
      preferedPreludes = catMaybes
        [ preludeModuleMay
        , join $ customPrelude <$> specialModeMay
        , defaultPrelude
        ]
  -- putLnMay $ "Prelude preferences: " <>  show preferedPreludes
  putLnMay $ "Expression: " <> unExpression expr
  hFlush stdout
  -- ================================================================ --
  case updateMode of
      -- ReplaceMode _mapFnSrc -> Legacy.replace config _mapFnSrc
      SearchMode            -> Legacy.search config
      _                     -> build preferedPreludes (unExpression expr)
                                 >>= either (reportError expr) treeOrStdin
  where
    putLnMay s
      | target == Target "-" = return ()
      | otherwise = putLn s

    treeOrStdin :: (ByteString -> ByteString) -> IO ()
    treeOrStdin f = case target of
      Target "-" -> processStdin f
      other      -> processTree other
      where
        processTree = S.mapM_ ( processWith updateMode f )
          . S.filter ( matchesAny compiledFilters )
          . S.filter ( not . ignored )
          . S.map fst
          . S.filter ( not . isDirectory . snd )
          . tree
          . unTarget

    defaultPrelude = Just "Refactorio.Prelude.Basic"

    allFilters = expandExtraFilters specialModeMay filenameFilters

    compiledFilters = map compileFilter . Set.toList $ allFilters

reportError :: Expression -> InterpreterError -> IO ()
reportError expr e = do
  nl
  putChunkLn $ chunk hdr  & fore c
  putChunkLn $ chunk msg & fore c
  nl
  where
    hdr :: Text = "Failed to compile expression:\n\n    " <> unExpression expr <> "\n"

    c = red  -- TODO: theme

    msg = case e of
      GhcException s        -> "GHC Exception:\n\n" <> s
      NotAllowed   s        -> "Not Allowed:\n\n"   <> s
      UnknownError s        -> "Unknown Error:\n\n" <> s
      WontCompile ghcErrors -> "GHC Errors:\n\n"    <> intercalate "\n" errors
        where
          errors = map errMsg ghcErrors

customPrelude :: SpecialMode -> Maybe FilePath
customPrelude m = Just $ "Refactorio.Prelude." <> show m

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

-- TODO: eliminate newline hack
processStdin :: (ByteString -> ByteString) -> IO ()
processStdin f = BS.interact ( (<> "\n") . f )

processWith :: UpdateMode -> (ByteString -> ByteString) -> FilePath -> IO ()
processWith updateMode f path = do
  (beforeBytes, afterBytes) <- (identity &&& f) <$> case path of
    "-" -> BS.getContents
    p   -> BS.readFile p
  if beforeBytes == afterBytes
    then putLn $ "** Unchanged: " <> pack path
    else handleChange (beforeBytes, afterBytes)
  where
    handleChange (beforeBytes, afterBytes) = do
      putLn $ "DELETE ME: CHANGING: " <> pack path
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
        ModifyMode -> do
          saveChanges afterBytes
          putLn $ "Changed: " <> show path
        PreviewMode ->
          showChanges "Preview" doc
        -- ReplaceMode _ -> panic "we should've been stopped before we got here!"
        ReviewMode -> do
          saveChanges afterBytes
          showChanges "Review" doc
        SearchMode -> panic "we should've been stopped before we got here!"
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
