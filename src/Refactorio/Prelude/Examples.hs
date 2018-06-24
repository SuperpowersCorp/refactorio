{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
     , version
     ) where

import           Refactorio.Prelude.Basic as Exports hiding ( (.=) )
import qualified Refactorio.Prelude       as RP

import           Data.Aeson
import           Data.Aeson.Lens          as Exports
import           Data.Text                as T
import           Refactorio.Helpers       as H
import           Safe                                       ( readMay )
import           System.Process

type Example = ( FilePath
               , ExampleData
               )

data ExampleData = ExampleData
  { _exampleName :: Text
  , _preText     :: Maybe Text
  , _postText    :: Maybe Text
  , _cmd         :: Text
  , _version     :: Int
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
    , "version" .= (exData ^. version)
    ] -- TODO: simplify
    ++ case exData ^. preText of
         Nothing -> []
         Just s  -> [ "pre" .= s ]
    ++ case exData ^. postText of
         Nothing -> []
         Just s  -> [ "post" .= s ]

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
    RP.sleep 0.9
    return $ exData & version +~ 1
  where
    fullCmd = replaceExe exe . replaceTarget target $ exData ^. cmd
    target  = "examples"                 -- TODO
    exe     = "stack exec refactorio --" -- TODO

replaceExe :: Text -> Text -> Text
replaceExe exe = T.intercalate exe . splitOn "$EXE"

replaceTarget :: Text -> Text -> Text
replaceTarget target = T.intercalate target . splitOn "$TARGET"

clearScreen :: IO ()
clearScreen = void $ system "clear"

makeScreenshotOf :: Int -> ExampleData -> IO ()
makeScreenshotOf winId exData = void $ readProcess "screencapture" args ""
  where
    args     = ["-t", "jpg", "-l", show winId, "-f", RP.unpack filename]
    filename = "examples/" <> exData ^. exampleName <> ".jpg"

getWindowId :: IO (Maybe Int)
getWindowId = readMay <$> readProcess "osascript" args ""
  where
    args = ["-e", "tell app \"iTerm2\" to id of window 1"]

generateIndex :: ByteString -> ByteString
generateIndex = panic "generateIndex undefined"
