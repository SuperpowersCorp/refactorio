{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Refactorio.Prelude.Examples
     ( module Exports
     , ExampleData
     , Example
     , H.yaml
     , exampleName
     , preText
     , postText
     , cmd
     , refactorioExamplesL
     , unsafeMakeScreenshot
     , version
     ) where

import           Refactorio.Prelude.Basic as Exports hiding ( (.=) )
import qualified Refactorio.Prelude       as RP

import           Data.Aeson
import           Data.Aeson.Lens          as Exports
import           Data.Text                as T
import           Refactorio.Helpers       as H
import           Safe                                       ( readMay )
import           System.IO.Unsafe                           ( unsafePerformIO )
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

-- TODO: Remove / move elsewhere
refactorioExamplesL :: Traversal' ByteString ExampleData
refactorioExamplesL = H.yaml . key "examples" . _Array . traverse . _JSON

-- TODO: Remove / move elsewhere
unsafeMakeScreenshot :: ExampleData -> ExampleData
unsafeMakeScreenshot exData = unsafePerformIO $ getWindowId >>= \case
  Nothing -> do
    RP.putLn "SCREENSHOT FAILED: COULD NOT FIND WINDOW ID"
    return exData
  Just winId -> do
    clearScreen
    RP.putLn $ "Example: " <> show (exData ^. exampleName)
    RP.putLn $ "% " <> fullCmd
    void . system . RP.unpack $ fullCmd
    makeScreenshot winId exData
    RP.sleep 0.5
    return $ exData & version +~ 1
  where
    fullCmd = replaceTarget target $ exData ^. cmd
    target = "/tmp/voltron"  -- TODO

replaceTarget :: Text -> Text -> Text
replaceTarget target = T.intercalate target . splitOn "$TARGET"

clearScreen :: IO ()
clearScreen = void $ system "clear"

makeScreenshot :: Int -> ExampleData -> IO ()
makeScreenshot winId exData = void $ readProcess "screencapture" args ""
  where
    args     = ["-l", show winId, RP.unpack filename]
    filename = "/tmp/deleteme/" <> exData ^. exampleName <> ".png"

getWindowId :: IO (Maybe Int)
getWindowId = readMay <$> readProcess "osascript" args ""
  where
    args = ["-e", "tell app \"iTerm2\" to id of window 1"]
