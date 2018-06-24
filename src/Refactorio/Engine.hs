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
process config@Config{..} = if updateMode == SearchMode
  then Legacy.search config
  else describeProcess config >> buildMapFn config >>= \case
    Left err -> reportError expr err
    Right  f -> if mixingStdin targets
      then reportCannotMixError (toList targets)
      else forM_ targets $
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

allFilters :: Config -> Set FilenameFilter
allFilters Config{..} = expandExtraFilters specialModeMay filenameFilters

mixingStdin :: NonEmpty Target -> Bool
mixingStdin xs = length xs > 1 && Target "-" `elem` toList xs

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
  when (beforeBytes /= afterBytes) $ do
    -- TODO: doublecheck (gh-6) here.
    case updateMode of
      AskMode -> changePrompt >>= \case
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
    saveChanges bs = do
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

buildMapFn :: Config -> IO (Either InterpreterError MappingFn)
buildMapFn config@Config{..}
  | monadic   = MapMFn <<$>> buildFn
  | otherwise = MapFn  <<$>> buildFn
  where
    buildFn :: Typeable a => IO (Either InterpreterError a)
    buildFn = build (preludesFrom config) (unExpression expr)

describeProcess :: Config -> IO ()
describeProcess config@Config{..} = do
  case specialModeMay of
    Nothing   -> return ()
    Just mode -> putLnMay $ "Special processing activated: " <> show mode
  putLnMay $ "Targets: " <> show (map unTarget targets)
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

preludesFrom :: Config ->[String]
preludesFrom Config{..} = catMaybes
  [ preludeModuleMay
  , join $ customPrelude <$> specialModeMay
  , defaultPrelude
  ]
  where
    defaultPrelude = Just "Refactorio.Prelude.Basic"

customPrelude :: SpecialMode -> Maybe FilePath
customPrelude m = Just $ "Refactorio.Prelude." <> show m

reportCannotMixError :: [Target] -> IO ()
reportCannotMixError targets = do
  nl
  putChunkLn $ chunk hdr & fore c
  putChunkLn $ chunk msg & fore c
  nl
  where
    hdr :: Text = "Invalid target set:\n\n    " <> show (map unTarget targets) <> "\n"
    msg :: Text = "You cannot mix stdin '-' with other targets."
    c = red  -- TODO: theme

reportError :: Expression -> InterpreterError -> IO ()
reportError expr e = do
  nl
  putChunkLn $ chunk hdr  & fore c
  putChunkLn $ chunk msg & fore c
  nl
  where
    hdr :: Text = "Failed to compile expression:\n\n    " <> unExpression expr <> "\n"

    msg = case e of
      GhcException s        -> "GHC Exception:\n\n" <> s
      NotAllowed   s        -> "Not Allowed:\n\n"   <> s
      UnknownError s        -> "Unknown Error:\n\n" <> s
      WontCompile ghcErrors -> "GHC Errors:\n\n"    <> intercalate "\n" errors
        where
          errors = map errMsg ghcErrors

    c = red  -- TODO: theme
