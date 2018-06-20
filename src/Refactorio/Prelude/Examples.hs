{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Refactorio.Prelude.Examples
     ( module Exports
     , ExampleData
     , Example
     , exampleName
     , preText
     , postText
     , cmd
     , refactorioExamples
     , unsafeMakeScreenshot
     ) where

import Refactorio.Prelude.Basic as Exports hiding ( (.=) )

import Data.Aeson
import Data.Aeson.Lens          as Exports

type Example = ( FilePath
               , ExampleData
               )

data ExampleData = ExampleData
  { _exampleName :: Text
  , _preText     :: Maybe Text
  , _postText    :: Maybe Text
  , _cmd         :: Text
  } deriving Show

makeLenses ''ExampleData

instance FromJSON ExampleData where
  parseJSON = withObject "ExampleData" $ \v -> ExampleData
    <$> v .: "name"
    <*> v .: "pre"
    <*> v .: "post"
    <*> v .: "cmd"

instance ToJSON   ExampleData where
  toJSON exData = object
    [ "name" .= (exData ^. exampleName)
    , "pre"  .= (exData ^. preText)
    , "post" .= (exData ^. postText)
    , "cmd"  .= (exData ^. cmd)
    ]

-- TODO: Remove / move elsewhere
refactorioExamples :: Traversal' ByteString ExampleData
refactorioExamples = key "examples" . _Array . traverse . _JSON

-- TODO: Remove / move elsewhere
unsafeMakeScreenshot :: ExampleData -> ExampleData
unsafeMakeScreenshot = panic "unsafeMakeScreenshot undefined"

-- needs to be a prism in both directions... ?

-- _YamlViaJson :: Prism' ByteString ByteString
-- _YamlViaJson = prism get set
--   where
--     get :: ByteString -> ByteString
--     get = undefined

--     set :: ByteString -> Either ByteString ByteString
--     set = undefined
