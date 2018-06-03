-- To be continued...

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Refactorio.BrickMain
     ( main
     ) where

import           Refactorio.Prelude

import           Brick                           hiding ( str )
import qualified Brick                      as B
import           Brick.Widgets.Border                   ( border
                                                        , borderWithLabel
                                                        )
import           Brick.Widgets.Border.Style             ( unicode )
import           Brick.Widgets.Center                   ( center )
import qualified Graphics.Vty               as V
import           Options.Applicative

data RefEvent = RefEvent
  deriving (Eq, Ord, Read, Show)

data RefName = RefName
  deriving (Eq, Ord, Read, Show)

data RefState = RefState
  { rootDirectory    :: FilePath
  , currentLensInput :: Text
  } deriving (Eq, Ord, Read, Show)

main :: IO ()
main = void $ execParser opts >>= defaultMain app
  where
    opts = info (parser <**> helper) mempty

    parser :: Parser RefState
    parser = RefState
      <$> argument str
            ( metavar "PROJECT_ROOT"
           <> value "."
            )
      <*> pure ""

    app :: App RefState RefName RefEvent
    app = App
      { appAttrMap      = attrMap'
      , appDraw         = appDraw'
      , appChooseCursor = chooseCursor'
      , appHandleEvent  = handleEvent'
      , appStartEvent   = startEvent'
      }

    attrMap' :: RefState -> AttrMap
    attrMap' = const $ attrMap V.defAttr []

    appDraw' :: RefState -> [Widget RefEvent]
    appDraw' st = [ withBorderStyle unicode
                    $ borderWithLabel (B.str "-==<] Refactorio [>==-")
                    $ center
                    $ border
                    $ B.str ("UI SOON (dir: " <> rootDirectory st <> ")")
                  <=> B.str ("input: " <> show (currentLensInput st))
                  ]

    chooseCursor' :: RefState
                  -> [CursorLocation RefEvent]
                  -> Maybe (CursorLocation RefEvent)
    chooseCursor' = neverShowCursor

    handleEvent' :: RefState
                 -> BrickEvent RefEvent RefName
                 -> EventM RefEvent (Next RefState)
    handleEvent' st _event = halt st -- continue st

    startEvent' :: RefState -> EventM RefEvent RefState
    startEvent' = return
