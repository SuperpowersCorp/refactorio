{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Refactorio.Experiments ( experiments ) where

import           Refactorio.Prelude                 hiding ( (<>)
                                                           , to
                                                           )

import           Control.Lens                       hiding ( pre
                                                           , (&)
                                                           )
import           Data.Monoid                               ( (<>) )
import qualified Data.ByteString              as BS
import qualified Data.Text                    as T
import           Language.Haskell.Exts
import           Language.Haskell.Exts.Prisms
import           Rainbow                                   ( Chunk
                                                           , chunk
                                                           , chunksToByteStrings
                                                           , fore
                                                           , red
                                                           , toByteStringsColors256
                                                           )

type Experiment = IO ()

experiments :: [Experiment]
experiments =
  [ experiment1
  , experiment2
  , experiment3
  ]

experiment1 :: IO ()
experiment1 = mapM_ print =<< findMatches moduleNameL examplePath
  where
    examplePath = "./src/Refactorio/Experiments.hs"

-- Same as experiment 1 but pretty print result w/ src.
experiment2 :: IO ()
experiment2 = mapM_ printPrettily =<< findMatches moduleNameL examplePath
  where
    examplePath = "./src/Refactorio/Experiments.hs"

    printPrettily :: SrcSpan -> IO ()
    printPrettily span = do
      -- TODO: obviously don't read the file each time
      putColorFrom span =<< readFile path
      where
        path = srcSpanFilename span

putColorFrom :: SrcSpan -> Text -> IO ()
putColorFrom span src = do
  mapM_ BS.putStr
    . chunksToByteStrings toByteStringsColors256
    . chunkify
    . take (srcSpanEndLine span - srcSpanStartLine span + 1)
    . drop (srcSpanStartLine span - 1)
    . T.lines
    $ src
  BS.putStr "\n"
  where
    chunkify :: [Text] -> [Chunk Text]
    chunkify = \case
      []     -> panic "how can we have no lines?"
      [x]    -> [pre, val, suf]
        where
          pre :: Chunk Text
          pre = (chunk . T.take startc $ x)

          val :: Chunk Text
          val = (chunk . T.take n . T.drop startc $ x)
            & fore red
            where
              n = endc - startc

          suf :: Chunk Text
          suf = (chunk . T.drop endc $ x)

          startc = srcSpanStartColumn span - 1
          endc   = srcSpanEndColumn span - 1

      xs@(x:_) -> firstLine ++ middleLines ++ lastLine
        where
          firstLine = [pre, firstVal]

          middleLines
            | length xs <  2 = panic "unpossible!"
            | length xs == 2 = []
            | otherwise = case initMay xs of
                            Nothing  -> panic "initMay failing - unpossible!"
                            Just xs' -> map chunk . drop 1 $ xs'

          lastLine    = [lastVal, post]

          pre      = chunk $ T.take (srcSpanStartColumn span) x
          firstVal = chunk $ T.drop (srcSpanStartColumn span) x
          lastVal  = chunk $ T.take (srcSpanEndColumn span)   lx
          post     = chunk $ T.drop (srcSpanEndColumn span)   lx

          lx = fromMaybe (panic "unpossible!") . lastMay $ xs

-- Same as experiment2 but lets do it on a whole directory
experiment3 :: IO ()
experiment3 = putLn "experiment3 not impl"
  --mapM_ print =<< findMatches exampleLens examplePath
  -- where
  --   examplePath = "./src"

moduleNameL :: Traversal' (Module SrcSpanInfo) [SrcSpan]
moduleNameL = _Module
  . _2
  . _Just
  . _ModuleHead
  . _2
  . _ModuleName
  . _1
  . srcInfoSpanL
  . godHelpMe

srcInfoSpanL :: Lens' SrcSpanInfo SrcSpan
srcInfoSpanL = lens srcInfoSpan $ \ssi sis -> ssi { srcInfoSpan = sis }

godHelpMe :: Lens' a [a]
godHelpMe = lens pure setErf
  where
    setErf :: a -> [a] -> a
    setErf _ []    = panic "can't godHelpMe an empty list"
    setErf _ (x:_) = x

findMatches :: ATraversal' (Module SrcSpanInfo) [SrcSpan] -> FilePath -> IO [SrcSpan]
findMatches trav path = do
  sourceString <- T.unpack <$> readFile path
  case parseFileContentsWithMode parseMode sourceString of
    ParseFailed srcLoc' err -> panic $ "ERROR at " <> show srcLoc' <> ": " <> show err
    ParseOk parsedMod       -> return . join $ toListOf (cloneTraversal trav) parsedMod
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
