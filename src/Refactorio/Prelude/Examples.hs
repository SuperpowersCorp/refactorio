{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Refactorio.Prelude.Examples
     ( module Exports
     , Example
     , ExampleData
     , H.yaml
     , cmd
     , exampleName
     , generateIndex
     , makeScreenshot
     , postText
     , preText
     , vers
     ) where

import           Refactorio.Prelude.Basic     as Exports hiding ( (.=)
                                                                , link
                                                                , para
                                                                )
import qualified Refactorio.Prelude           as RP

import           Data.Aeson
import           Data.Aeson.Lens              as Exports
import           Data.String                                    ( fromString )
import           Data.Text                    as T
import           Refactorio.Helpers           as H
import           Safe                                           ( readMay )
import           System.Directory                               ( removeFile
                                                                , renameFile
                                                                )
import           System.IO.Error                                ( isDoesNotExistError )
import           System.Posix.Files                             ( fileSize
                                                                , getFileStatus
                                                                )
import           System.Process
import           Text.Pandoc                                    ( Pandoc )
import           Text.Pandoc.Builder
import           Text.Pandoc.Writers.Markdown                   ( writeMarkdown )

type Example = ( FilePath
               , ExampleData
               )

data ExampleData = ExampleData
  { _exampleName :: Text
  , _preText     :: Maybe Text
  , _postText    :: Maybe Text
  , _cmd         :: Text
  , _vers        :: Int
  } deriving Show

makeLenses ''ExampleData

instance FromJSON ExampleData where
  parseJSON = withObject "ExampleData" $ \v -> ExampleData
    <$> v .:  "name"
    <*> v .:? "pre"
    <*> v .:? "post"
    <*> v .:  "cmd"
    <*> v .:  "version"

instance ToJSON   ExampleData where
  toJSON exData = object $
    [ "name"    .= (exData ^. exampleName)
    , "cmd"     .= (exData ^. cmd)
    , "version" .= (exData ^. vers)
    ] -- TODO: simplify
    ++ case exData ^. preText of
         Nothing -> []
         Just s  -> [ "pre" .= s ]
    ++ case exData ^. postText of
         Nothing -> []
         Just s  -> [ "post" .= s ]

generateIndex :: ByteString -> ByteString
generateIndex srcBytes = case eitherDecode . view (yaml . convert) $ srcBytes of
  Left  err -> panic $ "generateIndex FAILED: " <> view convert err
  Right val -> view convert . writeMarkdown writerOpts . makeIndex $ val
  where
    writerOpts = RP.def

    makeIndex :: Value -> Pandoc
    makeIndex val = doc $
         header 1 "Refactorio Examples"
      <> (mconcat . fmap makeExample . view (key "examples" . _JSON) $ val)

    makeExample :: ExampleData -> Blocks
    makeExample ex = header 2 (ex ^. (exampleName . convert . to text))
      <> prefix
      <> exampleOut
      <> suffix
      where
        exampleOut :: Blocks
        exampleOut = para $ image url title alt
          where
            alt   = text name
            name  = ex ^. (exampleName . convert)
            title = mempty
            url   = name ++ ".jpg"

        prefix = maybe mempty (para . fromString . view convert) $ ex ^. preText
        suffix = maybe mempty (para . fromString . view convert) $ ex ^. postText

makeScreenshot :: ExampleData -> IO ExampleData
makeScreenshot exData = getWindowId >>= \case
  Nothing -> do
    RP.putLn "SCREENSHOT FAILED: COULD NOT FIND WINDOW ID"
    return exData
  Just winId -> do
    clearScreen
    RP.putLn $ "Example: " <> show (exData ^. exampleName)
    RP.putLn $ "% " <> fullCmd
    void . system . RP.unpack $ fullCmd
    RP.sleep 0.2
    makeScreenshotOf winId exData
    -- return $ exData & vers +~ 1
    return exData
  where
    fullCmd = replaceExe exe . replaceTarget target $ exData ^. cmd
    target  = "examples"                 -- TODO
    exe     = "stack exec refactorio --" -- TODO

makeScreenshotOf :: Int -> ExampleData -> IO ()
makeScreenshotOf winId exData = do
  void $ readProcess "screencapture" args ""
  RP.sleep 0.2  -- TODO: can we get rid of this now?
  optimizeScreenshot . T.unpack $ filename
  where
    args     = ["-t", "jpg", "-l", show winId, "-f", RP.unpack filename]
    filename = "examples/" <> exData ^. exampleName <> ".jpg"

clearScreen :: IO ()
clearScreen = void . system $ "clear"

getWindowId :: IO (Maybe Int)
getWindowId = readMay <$> readProcess "osascript" args ""
  where
    args = ["-e", "tell app \"iTerm2\" to id of window 1"]

optimizeScreenshot :: FilePath -> IO ()
optimizeScreenshot path = do
  jpegTran "optimize"    path optPath
  jpegTran "progressive" path proPath
  keepSmallest path [path, optPath, proPath]
  where
    optPath = path ++ "opt."
    proPath = path ++ "pro."

    jpegTran :: [Char] -> FilePath -> FilePath -> IO ()
    jpegTran mode inPath outPath = void $ flip (readProcess "jpegtran") ""
      [ "-copy"
      , "none"
      , "-" ++ mode
      , "-outfile"
      , outPath
      , inPath
      ]

    keepSmallest :: FilePath -> [FilePath] -> IO ()
    keepSmallest _ [] = return ()
    keepSmallest destPath candidates = do
      sizes <- mapM (fmap fileSize . getFileStatus) candidates
      case fmap snd . sortOn fst $ RP.zip sizes candidates of
        [] -> return ()
        (winner:losers) -> do
          promote   winner
          eliminate losers
      where
        promote :: FilePath -> IO ()
        promote p
          | p == destPath = return ()
          | otherwise     = renameFile p destPath

        eliminate :: [FilePath] -> IO ()
        eliminate = mapM_ removeIfExists . RP.filter (/= destPath)

        removeIfExists :: FilePath -> IO ()
        removeIfExists fileName = removeFile fileName `catch` handleExists
          where
            handleExists e
              | isDoesNotExistError e = return ()
              | otherwise             = throwIO e

replaceExe :: Text -> Text -> Text
replaceExe exe = T.intercalate exe . splitOn "$EXE"

replaceTarget :: Text -> Text -> Text
replaceTarget target = T.intercalate target . splitOn "$TARGET"
