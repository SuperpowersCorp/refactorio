{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

module Refactorio.Experiments ( experiments ) where

import Refactorio.Prelude hiding ( to )

import Control.Lens
import Data.Text                    ( unpack )
import Language.Haskell.Exts        ( Language( Haskell2010 )
                                    , Module
                                    , Extension      ( EnableExtension )
                                    , ParseResult    ( ParseFailed
                                                     , ParseOk
                                                     )
                                    , SrcSpanInfo
                                    , SrcSpan
                                    , KnownExtension ( OverloadedStrings
                                                     , RankNTypes
                                                     )
                                    , baseLanguage
                                    , defaultParseMode
                                    , ignoreLanguagePragmas
                                    , extensions
                                    , parseFilename
                                    , parseFileContentsWithMode
                                    )
import Language.Haskell.Exts.SrcLoc
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
experiments = [experiment1]

experiment1 :: IO ()
experiment1 = mapM_ print =<< findMatches exampleLens examplePath
  where
    examplePath = "./src/Refactorio/Experiments.hs"

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
