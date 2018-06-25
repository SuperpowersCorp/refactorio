{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Refactorio.Engine ( process ) where

import           Refactorio.Prelude             as P      hiding ( (<>) )
import qualified Streaming.Prelude              as S

import           Data.Algorithm.DiffContext
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as C8
import qualified Data.Set                       as Set
import qualified Data.Text                      as T
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
process config@Config{..}
  | mixingStdin targets      = errorCannotMix . toList $ targets
  | updateMode == SearchMode = Legacy.search config
  | otherwise                = describeProcess config >> buildMapFn config >>= \case
      Left err -> errorInExpression expr err
      Right  f -> forM_ targets $
                    S.mapM_ (processFile updateMode f)
                      . S.filter ( matchesAny compiledFilters )
                      . S.filter ( not . ignored )
                      . S.map fst
                      . S.filter ( not . isDirectory . snd )
                      . tree
                      . unTarget
  where
    compiledFilters = map compileFilter . Set.toList . allFilters $ config

    -- TODO: read .*ignore files from the target dir down to the current file, caching
    --       along the way, etc. but for now...
    ignored :: FilePath -> Bool
    ignored path = any ($ path)
      [ (`contains`   ".stack-work")
      , (`startsWith` ".git/")
      , (`contains`   "/.git/")
      ]

processFile :: UpdateMode -> MappingFn -> FilePath -> IO ()
processFile updateMode (MapFn   f) path = processFile updateMode (MapMFn $ return . f) path
processFile updateMode (MapMFn mf) path = do
  beforeBytes <- case path of
    "-"   -> BS.hGetContents stdin
    other -> BS.readFile other
  afterBytes <- mf beforeBytes
  let doc         = prettyContextDiff beforeName afterName elPrint diff'
      diff'       = getContextDiff ctxLines beforeLines afterLines
      beforeLines = C8.lines beforeBytes
      afterLines  = C8.lines afterBytes
  if path == "-"
    then saveChanges afterBytes
    else when (beforeBytes /= afterBytes) $ case updateMode of
      -- TODO: doublecheck (gh-6) here.
      AskMode -> showChanges "Preview" doc >> changePrompt >>= \case
        AcceptChange -> saveChanges afterBytes >> putLn ("Saved: " <> show path)
        RejectChange -> putLn "File unchanged."
        QuitChanges  -> putLn "Exiting at user's request." >> exitSuccess
      ModifyMode  -> saveChanges afterBytes >> putLn ("Changed: " <> show path)
      Nope        -> putLn "File unchanged."
      PreviewMode -> showChanges "Preview" doc
      ReviewMode  -> saveChanges afterBytes >> showChanges "Review" doc
      SearchMode  -> panic "we should not be in processFile in SearchMode"
  where
    ctxLines = 2

    saveChanges :: ByteString -> IO ()
    saveChanges bs
      | path == "-" = BS.putStr bs
      | otherwise = do
          putLn $ "Saving changes to " <> T.pack path
          BS.writeFile path bs

    showChanges :: Text -> Doc -> IO ()
    showChanges label doc = do
      nl
      putChunkLn $ (chunk . unpack $ label <> " of changes to: " <> show path) & fore yellow
      putChunkLn $ chunk divider & fore yellow
      colorDisplay doc
      where
        divider = T.pack . replicate 64 $ '='

    beforeName :: Doc
    beforeName = PP.text $ path <> " BEFORE"

    afterName :: Doc
    afterName = PP.text $ path <> " AFTER"

    elPrint :: ByteString -> Doc
    elPrint = PP.text . unpack . decodeUtf8

    colorDisplay :: Doc -> IO ()
    colorDisplay = mapM_ colorLn . T.lines . render'
      where
        colorLn :: Text -> IO ()
        colorLn s
          | s `startsWith` "-" = putChunkLn $ chunk s & fore red
          | s `startsWith` "+" = putChunkLn $ chunk s & fore green
          | otherwise          = putChunkLn $ chunk s & fore grey

    render' :: Doc -> Text
    render' = T.pack . PP.render

allFilters :: Config -> Set FilenameFilter
allFilters Config{..}
  | isStdin   = Set.empty
  | otherwise = expandExtraFilters specialModeMay filenameFilters
  where
    isStdin = targets == Target "-" :| []

buildMapFn :: Config -> IO (Either InterpreterError MappingFn)
buildMapFn config@Config{..}
  | monadic   = MapMFn <<$>> buildFn
  | otherwise = MapFn  <<$>> buildFn
  where
    buildFn :: Typeable a => IO (Either InterpreterError a)
    buildFn = build (preludesFrom config) (unExpression expr)

changePrompt :: IO ChangeChoice
changePrompt = do
  putStr ("Accept change (Y/N/Q)? " :: Text)
  hFlush stdout
  (T.toLower <$> getLn) >>= \case
    "y"    -> return AcceptChange
    "yes"  -> return AcceptChange
    "n"    -> return RejectChange
    "no"   -> return RejectChange
    "q"    -> return QuitChanges
    "quit" -> return QuitChanges
    _      -> changePrompt

customPrelude :: SpecialMode -> Maybe FilePath
customPrelude m = Just $ "Refactorio.Prelude." <> show m

describeProcess :: Config -> IO ()
describeProcess config@Config{..} = do
  case specialModeMay of
    Nothing   -> return ()
    Just mode -> putLnMay $ "Special processing activated: " <> show mode
  putLnMay $ "Targets: " <> show (map unTarget . toList $ targets)
  putLnMay $ if null filters
    then "No filters."
    else "Filters: " <> show (map unFilenameFilter . Set.toList $ filters)
  putLnMay $ "Preludes: " <> show (preludesFrom config)
  putLnMay $ "Expression: " <> unExpression expr
  hFlush stdout
  where
    filters =  allFilters config

    putLnMay s
      | toList targets == [Target "-"] = return ()
      | otherwise                      = putLn s

mixingStdin :: NonEmpty Target -> Bool
mixingStdin xs = length xs > 1 && Target "-" `elem` toList xs

preludesFrom :: Config ->[String]
preludesFrom Config{..} = catMaybes
  [ preludeModuleMay
  , join $ customPrelude <$> specialModeMay
  , defaultPrelude
  ]
  where
    defaultPrelude = Just "Refactorio.Prelude.Basic"

reportError :: Text -> Text -> IO ()
reportError hdr msg = do
  nl
  putChunkLn $ chunk hdr & fore c
  putChunkLn $ chunk msg & fore c
  nl
  where
    c = red  -- TODO: theme

errorCannotMix :: [Target] -> IO ()
errorCannotMix targets = reportError hdr msg
  where
    hdr = "Invalid target set:\n\n    " <> show (map unTarget targets) <> "\n"
    msg = "You cannot mix stdin '-' with other targets."

errorInExpression :: Expression -> InterpreterError -> IO ()
errorInExpression expr e = reportError hdr msg
  where
    hdr = "Failed to compile expression:\n\n    " <> unExpression expr <> "\n"

    msg = T.pack $ case e of
      GhcException s        -> "GHC Exception:\n\n" <> s
      NotAllowed   s        -> "Not Allowed:\n\n"   <> s
      UnknownError s        -> "Unknown Error:\n\n" <> s
      WontCompile ghcErrors -> "GHC Errors:\n\n"    <> intercalate "\n" errors
        where
          errors = map errMsg ghcErrors
