{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Refactorio.Search where

import           Refactorio.Prelude                hiding ( (<>) )
import qualified Streaming.Prelude            as S

import           Control.Lens                 as A hiding ( (&)
                                                          , pre
                                                          )
import qualified Data.List                    as L
import           Data.Monoid                              ( (<>) )
import           Data.Text                    as T hiding ( span )
import           Language.Haskell.Exts        as Y hiding ( Style )
import           Language.Haskell.Interpreter      hiding ( OverloadedStrings
                                                          , RankNTypes
                                                          )
import           Rainbow.Extra                     hiding ( (&) )
import           Refactorio.Config
import           Refactorio.InterPrelude                  ( srcSpanInfoL )
import           Refactorio.Theme
import           Streaming.Files                          ( tree )

byLens :: Config -> IO ()
byLens Config {..} = makeLens lensText >>= \case
  Left err -> displayError theme err
  Right trav -> S.mapM_ (showMatches trav)
    . S.chain reportFile
    . S.filter (\(p, _) -> ".hs" `L.isSuffixOf` p && not (".stack-work" `L.isInfixOf` p))
    . tree
    $ projectRoot
    where
      showMatches t (p, _) = findMatches t p
                               >>= mapM_ (\x -> printPrettily theme x >> newLine)

      reportFile (p, _) = putChunkLn (chunk (pack p) & filename theme)

displayError :: Theme -> InterpreterError -> IO ()
displayError theme = \case
  GhcException msg -> putChunkLn
    $ chunk ("GHC Exception:\n  " <> (show msg :: Text))
    & errorColor theme
  NotAllowed msg -> putChunkLn
    $ chunk ("Not allowed:\n  " <> (show msg :: Text))
    & errorColor theme
  UnknownError msg -> putChunkLn
    $ chunk ("An unknown error has occured:\n  " <> (show msg :: Text))
    & errorColor theme
  WontCompile errors -> do
    putChunkLn
      $ chunk ("Encountered " <> show (L.length errors) <> " errors in the definition of your lens:")
      & errorColor theme
    mapM_ (\e -> putChunkLn . errorColor theme . chunk . pack . errMsg $ e) errors

makeLens :: Text -> IO (Either InterpreterError
                        (ATraversal' (Module SrcSpanInfo) SrcSpanInfo))
makeLens s = runInterpreter $ do
  loadModules
    [ "/Users/john/.refactorio/InterPrelude.hs"
    ]
  setImports
    [ "Prelude"
    , "Control.Lens"
    , "Data.Data.Lens"
    , "Language.Haskell.Exts"
    , "Language.Haskell.Exts.Prisms"
    , "Refactorio.InterPrelude"
    ]
  interpret (unpack s) infer

findMatches :: ATraversal' (Module SrcSpanInfo) SrcSpanInfo -> FilePath -> IO [SrcSpanInfo]
findMatches trav path = do
  sourceString <- unpack <$> readFile path
  case parseFileContentsWithMode (parseMode path) sourceString of
    ParseFailed srcLoc' err -> panic $ "ERROR at " <> show srcLoc' <> ": " <> show err
    ParseOk parsedMod       -> return $ toListOf (cloneTraversal trav) parsedMod

printPrettily :: Theme -> SrcSpanInfo -> IO () -- TODO: don't read the file each time
printPrettily theme spanInfo = putColorFrom theme span =<< readFile (srcSpanFilename span)
    where
      span = spanInfo ^. srcSpanInfoL

putColorFrom :: Theme -> SrcSpan -> Text -> IO ()
putColorFrom theme span src = do
  mapM_ putStr
    . chunksToByteStrings toByteStringsColors256
    . chunkify
    . L.take (srcSpanEndLine span - srcSpanStartLine span + 1)
    . L.drop (srcSpanStartLine span - 1)
    . T.lines
    $ src
  putStr ("\n" :: ByteString)
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
            & match theme
            where
              n = endc - startc

          suf :: Chunk Text
          suf = (chunk . T.drop endc $ x)

          startc = srcSpanStartColumn span - 1
          endc   = srcSpanEndColumn span - 1

      xs@(x:_) -> firstLine ++ middleLines ++ lastLine
        where
          firstLine = [pre, firstVal]

          middleLines                        -- TODO: we need 'total' concentwation
            | L.length xs <  2 = panic "unpossible!"
            | L.length xs == 2 = []
            | otherwise = case initMay xs of
                            Nothing  -> panic "initMay failing - unpossible!"
                            Just xs' -> fmap chunk . L.drop 1 $ xs'

          lastLine    = [lastVal, post]

          pre      = chunk $ T.take (srcSpanStartColumn span) x
          firstVal = chunk $ T.drop (srcSpanStartColumn span) x
          lastVal  = chunk $ T.take (srcSpanEndColumn span)   lx
          post     = chunk $ T.drop (srcSpanEndColumn span)   lx

          lx = fromMaybe (panic "unpossible!") . lastMay $ xs

-- TODO: finish
parseMode :: FilePath -> ParseMode
parseMode path = defaultParseMode
  { baseLanguage          = Haskell2010
  , ignoreLanguagePragmas = False
  , extensions            = configuredExtensions
  , parseFilename         = path
  }
  where
    configuredExtensions = extensions defaultParseMode ++ tempManualExtensions
    tempManualExtensions = fmap EnableExtension
      [ OverloadedStrings
      , RankNTypes
      ]
