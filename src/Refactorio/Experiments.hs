{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

module Refactorio.Experiments ( experiments ) where

import Refactorio.Prelude hiding ( to )

import Control.Lens
import Data.Text                   ( lines
                                   , unlines
                                   , unpack
                                   )
import Language.Haskell.Exts
import Language.Haskell.Exts.Prisms

-- Let's see if we can write a function that takes a lens and a single haskell
-- source file and shows any matches.

srcInfoSpanL :: Lens' SrcSpanInfo SrcSpan
srcInfoSpanL = lens srcInfoSpan $ \ssi sis -> ssi { srcInfoSpan = sis }

godHelpMe :: Lens' a [a]
godHelpMe = lens pure setErf
  where
    setErf :: a -> [a] -> a
    setErf _ []    = panic "can't godHelpMe an empty list"
    setErf _ (x:_) = x

type Experiment = IO ()

experiments :: [Experiment]
experiments =
  [ experiment1
  , experiment2
  , experiment3
  ]

experiment1 :: IO ()
experiment1 = mapM_ print =<< findMatches exampleLens examplePath
  where
    examplePath = "./src/Refactorio/Experiments.hs"

-- Same as experiment 1 but pretty print result w/ src.
experiment2 :: IO ()
experiment2 = mapM_ printPrettily =<< findMatches exampleLens examplePath
  where
    examplePath = "./src/Refactorio/Experiments.hs"

    printPrettily :: SrcSpan -> IO ()
    printPrettily span = do
      -- TODO: obviously don't read the file each time
      putLn . showFrom span =<< readFile path
      where
        path = srcSpanFilename span

showFrom :: SrcSpan -> Text -> Text
showFrom span = unlines
  . map showPair
  . zip [(srcSpanStartLine span)..]
  . take (srcSpanEndLine span - srcSpanStartLine span + 1)
  . drop (srcSpanStartLine span - 1)
  . lines
  where
    showPair :: (Int, Text) -> Text
    showPair (n, s) = "Line " <> show n <> ". " <> s

-- Same as experiment2 but lets do it on a whole directory
experiment3 :: IO ()
experiment3 = putLn "experiment3 not impl"
  --mapM_ print =<< findMatches exampleLens examplePath
  -- where
  --   examplePath = "./src"

exampleLens :: Traversal' (Module SrcSpanInfo) [SrcSpan]
exampleLens = _Module
  . _2
  . _Just
  . _ModuleHead
  . _2
  . _ModuleName
  . _1
  . srcInfoSpanL
  . godHelpMe

findMatches :: ATraversal' (Module SrcSpanInfo) [SrcSpan] -> FilePath -> IO [SrcSpan]
findMatches trav path = do
  sourceString <- unpack <$> readFile path
  case parseFileContentsWithMode parseMode sourceString of
    ParseFailed srcLoc' err -> panic $ "ERROR at " <> show srcLoc' <> ": " <> show err
    ParseOk parsedMod -> return . join $ toListOf (cloneTraversal trav) parsedMod
      --return $ parsedMod ^# optic
  where
    parseMode = defaultParseMode
      { baseLanguage          = Haskell2010
      , ignoreLanguagePragmas = False
      , extensions            = configuredExtensions
      , parseFilename         = path
      }

    configuredExtensions = extensions defaultParseMode ++ tempManualExtensions

    tempManualExtensions = map EnableExtension
      [ OverloadedStrings
        -- , NoImplicitPrelude
      , RankNTypes
      ]
