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

import Refactorio.Prelude.Basic as Exports hiding ( (.=) )

import Refactorio.Helpers       as H
import Data.Aeson
import Data.Aeson.Lens          as Exports
import System.IO.Unsafe                           ( unsafePerformIO )
import qualified Data.ByteString as TempBS

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

_test1 :: IO ()
_test1 = do
  bs <- TempBS.readFile "examples/examples.yaml"
  print $ bs & refactorioExamplesL %~ unsafeMakeScreenshot

-- TODO: Remove / move elsewhere
unsafeMakeScreenshot :: ExampleData -> ExampleData
unsafeMakeScreenshot exData = unsafePerformIO $ do
  putStrLn ("================ WOULD SCREENSHOT HERE ================" :: Text)
  return (exData & version +~ 1)
