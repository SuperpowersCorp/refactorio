{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Refactorio.Experiments
     ( experiment1
     , experiment2
     , experiment3
     , experiment4
     , experiment5
     ) where

import           Refactorio.Prelude                 hiding ( (<>)
                                                           , Proxy
                                                           )

import           Control.Lens                       hiding ( pre
                                                           , (&)
                                                           )
import           Data.List                                 ( isInfixOf
                                                           , isSuffixOf
                                                           )
import           Data.Monoid                               ( (<>) )
import qualified Data.ByteString              as BS
import qualified Data.Text                    as T
import           Language.Haskell.Exts
import           Language.Haskell.Exts.Prisms
import           Language.Haskell.Interpreter              ( InterpreterError
                                                           , eval
                                                           , infer
                                                           , interpret
                                                           , loadModules
                                                           , runInterpreter
                                                           , setImports
                                                           )
import           Rainbow                                   ( Chunk
                                                           , chunk
                                                           , chunksToByteStrings
                                                           , fore
                                                           , red
                                                           , toByteStringsColors256
                                                           )
-- import           Streaming
import qualified Streaming.Prelude            as S
import           Streaming.Files                           ( FileInfo
                                                           , tree
                                                           )
import           Refactorio.InterPrelude                   ( srcInfoSpanL
                                                           , unListL
                                                           )

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
printPrettily span =
  -- TODO: obviously don't read the file each time
  putColorFrom span =<< readFile path
    where
      path = srcSpanFilename span

printArb :: Text -> IO ()
printArb = putLn . ("ARB: " <>)

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

experiment3 :: FilePath -> IO ()
experiment3 = S.mapM_ showMatches
  . S.filter (not . (".stack-work" `isInfixOf`) . fst)
  . S.filter ((".hs" `isSuffixOf`) . fst)
  . tree
  where
    showMatches :: FileInfo -> IO ()
    showMatches (path, _) = mapM_ printPrettily =<< findMatches moduleNameL path

experiment4 :: IO ()
experiment4 = myEval "2 + 2" >>= print

myEval :: Text -> IO (Either InterpreterError Text)
myEval s = runInterpreter $ do
  setImports ["Prelude"]
  T.pack <$> eval (T.unpack s)

experiment5 :: FilePath -> Text -> IO ()
experiment5 = searchByLens

searchByLens :: FilePath -> Text -> IO ()
searchByLens path s = makeLens s >>= \case
  Left err -> print err
  Right trav -> S.mapM_ (showMatches trav)
    . S.filter (\(p, _) -> ".hs" `isSuffixOf` p && not (".stack-work" `isInfixOf` p))
    . tree
    $ path
    where
      showMatches t (p, _) = mapM_ printPrettily =<< findMatches t p

-- eg.
-- _getByLens "." "_Module . biplate . _FieldUpdate . _1 . srcInfoSpanL . unListL"

_getByLens :: FilePath -> Text -> IO [[SrcSpan]]
_getByLens path s = makeLens s >>= \case
  Left err -> panic . show $ err
  Right trav ->
    fmap S.fst'
      . S.toList
      . S.mapM (showMatches trav)
      . S.filter (\(p, _) -> ".hs" `isSuffixOf` p && not (".stack-work" `isInfixOf` p))
      . tree
      $ path
    where
      showMatches t (p, _) = findMatches t p

_searchByArbLens :: FilePath -> Text -> IO ()
_searchByArbLens path s = makeLens s >>= \case
  Left err -> print err
  Right trav -> do
    let showMatches (p, _) = mapM_ printArb =<< findArbMatches p trav
    S.mapM_ showMatches
      . S.filter (not . (".stack-work" `isInfixOf`) . fst)
      . S.filter ((".hs" `isSuffixOf`) . fst)
      . tree
      $ path

_testMakeLens :: IO (Either InterpreterError (ATraversal' (Module SrcSpanInfo) [SrcSpan]))
_testMakeLens = makeLens
  "_Module . _2 . _Just . _ModuleHead . _2 . _ModuleName . _1 . srcInfoSpanL . unListL"

_showIfLeft :: Show a => Either a b -> IO ()
_showIfLeft (Left x)  = putLn $ "failed: " <> show x
_showIfLeft (Right _) = putLn "got the traversal!"

makeLens :: Text -> IO (Either InterpreterError
                        (ATraversal' (Module SrcSpanInfo) [SrcSpan]))
makeLens s = runInterpreter $ do
  loadModules
    [ "src/Refactorio/InterPrelude.hs"
    ]
  setImports
    [ "Prelude"
    , "Control.Lens"
    , "Data.Data.Lens"
    , "Language.Haskell.Exts"
    , "Language.Haskell.Exts.Prisms"
    , "Refactorio.InterPrelude"
    ]
  interpret (T.unpack s) infer

moduleNameL :: Traversal' (Module SrcSpanInfo) [SrcSpan]
moduleNameL = _Module
  . _2
  . _Just
  . _ModuleHead
  . _2
  . _ModuleName
  . _1
  . srcInfoSpanL
  . unListL

findMatches :: ATraversal' (Module SrcSpanInfo) [SrcSpan] -> FilePath -> IO [SrcSpan]
findMatches trav path = do
  sourceString <- T.unpack <$> readFile path
  case parseFileContentsWithMode (parseMode path) sourceString of
    ParseFailed srcLoc' err -> panic $ "ERROR at " <> show srcLoc' <> ": " <> show err
    ParseOk parsedMod       -> return . join $ toListOf (cloneTraversal trav) parsedMod

findArbMatches :: Show a => FilePath -> ATraversal' (Module SrcSpanInfo) a -> IO [Text]
findArbMatches path trav = do
  sourceString <- T.unpack <$> readFile path
  case parseFileContentsWithMode (parseMode path) sourceString of
    ParseFailed srcLoc' err -> panic $ "ERROR at " <> show srcLoc' <> ": " <> show err
    ParseOk parsedMod -> return . map show . toListOf (cloneTraversal trav) $ parsedMod

parseMode :: FilePath -> ParseMode
parseMode path = defaultParseMode
  { baseLanguage          = Haskell2010
  , ignoreLanguagePragmas = False
  , extensions            = configuredExtensions
  , parseFilename         = path
  }
  where
    configuredExtensions = extensions defaultParseMode ++ tempManualExtensions

    tempManualExtensions = map EnableExtension
      [ OverloadedStrings
        -- , NoImplicitPrelude
      , RankNTypes
      ]
