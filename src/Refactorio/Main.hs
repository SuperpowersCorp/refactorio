{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Refactorio.Main ( main ) where

import Refactorio.Prelude  as P       hiding ( (<>) )

import Data.Set            as Set
import Data.String (String)
import Data.Text           as Text
import Options.Applicative hiding (prefs)
import Refactorio.Config
import X.Rainbow--                      hiding ( (&) )

-- CURRENT TARGET:   refio --haskell view "__Module.biplate._Int" & "(+32)"

main :: IO ()
main = void $ customExecParser prefs opts >>= apphrend
  where
    prefs = defaultPrefs
      { prefShowHelpOnError = True
      , prefShowHelpOnEmpty = True
      }

    opts = info (parser <**> helper) $ fullDesc
           <> header   "Refactorio - Optical Refactoring Tool"
           <> progDesc "Zen and the art of optical file maintenance."

parser :: Parser Config
parser = prefixConfigParser
  -- <|> infixConfigParser
  where
    prefixConfigParser :: Parser Config
    prefixConfigParser = Config
      <$> filenameFilterSetParser
      <*> lensOperatorParser
      <*> lensTextParser
      <*> optional mapFnMapParser
      <*> previewParser
      <*> targetParser

    -- infixConfigParser :: Parser Config
    -- infixConfigParser = do
    --   filenameFilterSetParser' <- filenameFilterSetParser
    --   lensTextParser'          <- lensTextParser
    --   lensOperatorParser'      <- lensOperatorParser
    --   optionalMapFnMapParser'  <- optional mapFnMapParser
    --   previewParser'           <- previewParser
    --   targetParser'            <- targetParser
    --   return $ Config
    --     filenameFilterSetParser'
    --     lensOperatorParser'
    --     lensTextParser'
    --     optionalMapFnMapParser'
    --     previewParser'
    --     targetParser'

    _ = Config :: Set FilenameFilter
               -> LensOperator
               -> LensText
               -> Maybe MapFnText
               -> PreviewMode
               -> Target
               -> Config

    _fromInfix :: Set FilenameFilter
               -> LensText
               -> LensOperator
               -> Maybe MapFnText
               -> PreviewMode
               -> Target
               -> Config
    _fromInfix filts = flip (Config filts)

previewParser :: Parser PreviewMode
previewParser = pure PreviewModeEnabled

lensTextParser :: Parser LensText
lensTextParser = LensText . pack <$> argument str
  ( metavar "TRAVERSAL"
 <> help    "Traversal' FileInfo SrcSpanInfo (for now)"
  )

mapFnMapParser :: Parser MapFnText
mapFnMapParser = MapFnText . pack <$> argument str
   ( metavar "F"
  <> help "the function to apply with the lens"
   )

targetParser :: Parser Target
targetParser = Target <$> strOption
  ( long        "target"
 <> short       't'
 <> metavar     "TARGET"
 <> help        "a file/directory to search/replace"
 <> showDefault
 <> value       "/tmp/voltron ." -- TODO
  )

lensOperatorParser :: Parser LensOperator
lensOperatorParser = -- infixOperatorParser <|>
  prefixOperatorParser

-- infixOperatorParser :: Parser LensOperator
-- infixOperatorParser = combineInfixOps
--   [ (Over,  "%~", "over")
--   , (Plus,  "+~", "plus")
--   , (Set,   ".~", "set")
--   , (Times, "*~", "times")
--   , (View,  "^.", "view")
--   ]
--   where
--     combineInfixOps :: [(LensOperator, Text, Text)] -> Parser LensOperator
--     combineInfixOps = P.foldl' f z
--       where
--         f :: Parser LensOperator -> (LensOperator, Text, Text) -> Parser LensOperator
--         f _ _ = g <$> argument str mempty

--         g :: String -> LensOperator
--         g = panic "g undefined"

--         z :: Parser LensOperator
--         z = pure View <* argument str mempty

prefixOperatorParser :: Parser LensOperator
prefixOperatorParser = combinePrefixOps
  [ prefixOp "%~" "over"  Over
  , prefixOp "+~" "plus"  Plus
  , prefixOp ".~" "set"   Set
  , prefixOp "*~" "times" Times
  , prefixOp "^." "view"  View
  ]
  where
    combinePrefixOps :: [Parser LensOperator] -> Parser LensOperator
    combinePrefixOps = fromMaybe (panic "combinePrefixOps Nothing undefined")
      . headMay -- TODOx

    prefixOp :: String -> String -> LensOperator -> Parser LensOperator
    prefixOp sigil _name op = something op sigil $ argument str mempty

    something :: LensOperator -> String -> Parser String -> Parser LensOperator
    something _lo _s parser' = foo <$> parser'
      where
        foo :: String -> LensOperator
        foo "over"  = Over
        foo "plus"  = Plus
        foo "set"   = Plus
        foo "times" = Plus
        foo "view"  = Plus
        foo x       = panic $ "invalid! ==> " <> pack x

filenameFilterSetParser :: Parser (Set FilenameFilter)
filenameFilterSetParser = pure $ Set.fromList [DotPattern "hs"] -- TODO

apphrend :: Config -> IO ()
apphrend config@Config{..} = do
  putLn "attempting apprhension..."
  print config
