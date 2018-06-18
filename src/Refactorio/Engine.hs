{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Refactorio.Engine ( process ) where

import           Refactorio.Prelude             as P    hiding ( (<>) )
import qualified Streaming.Prelude              as S

import           Data.Algorithm.DiffContext
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as C8
import qualified Data.List                      as List
import qualified Data.Set                       as Set
import           Data.Text                                     ( pack )
import           Refactorio.FilenameFilter
import           Refactorio.Types
import           System.Posix.Files
import           Text.PrettyPrint               as PP   hiding ( (<>) )
import           X.Language.Haskell.Interpreter                ( build )
import           X.Rainbow
import           X.Streaming.Files                             ( tree )
import System.IO

-- CURRENT TARGET:   refio --haskell view "__Module.biplate._Int" & "(+32)"

process :: Config -> IO ()
process Config{..} = do
  putLn $ "Target: " <> show (unTarget target)
  putLn $ "Filters: " <> show (map unFilenameFilter . Set.toList $ filenameFilters)
  case specialMode of
    Nothing -> return ()
    Just mode -> putLn $ "Special mode activated: " <> show mode
  putLn $ "Expression: " <> show (unExpression expr)
  build (unExpression expr) >>= either (panic . show) process'
  where
    process' f = do
      -- TODO: flush
      hFlush stdout
      S.mapM_ ( processWith f )
        . S.chain (putLn . ("DEBUG 2: " <>) . show)
        -- . S.filter ( matchesAny compiledFilters )
        . S.filter ( matchesAnyString . map (unpack . unFilenameFilter) . Set.toList $ filenameFilters )
        . S.chain (putLn . ("DEBUG 1: " <>) . show)
        . S.filter ( not . ignored )
        . S.map fst
        . S.filter ( not . isDirectory . snd )
        . tree
        . unTarget
        $ target

    compiledFilters = map compileFilter . Set.toList $ filenameFilters

-- TODO: read .*ignore files from the target dir down to the current file, caching
--       along the way, etc. but for now...
ignored :: FilePath -> Bool
ignored = (".stack-work" `List.isInfixOf`)

processWith :: (ByteString -> ByteString) -> FilePath -> IO ()
processWith f path = do
  (beforeBytes, afterBytes) <- (identity &&& f) <$> BS.readFile path
  if beforeBytes == afterBytes
    then putLn $ "** Unchanged: " <> pack path
    else do
      let beforeLines = C8.lines beforeBytes
          afterLines  = C8.lines afterBytes
          diff'       = getContextDiff ctxLines beforeLines afterLines
          doc         = prettyContextDiff beforeName afterName elPrint diff'
      putLn $ "** Changed: " <> show path
      putLn "================================================================"
      display doc
  where
    beforeName :: Doc
    beforeName = PP.text $ path <> " BEFORE"

    afterName :: Doc
    afterName = PP.text $ path <> " AFTER"

    ctxLines :: Int
    ctxLines = 2

    elPrint :: ByteString -> Doc
    elPrint = PP.text . unpack . decodeUtf8

    render' :: Doc -> Text
    render' = pack . PP.render

    display :: Doc -> IO ()
    display = putLn . render'

    -- TODO: altDisplay that putChunkLn's according to - or + in front

_test1 :: IO ()
_test1 = do
  putLn "== test1 start =="
  processWith BS.reverse path
  putLn "== test1 complete =="
  where
    path = "/tmp/voltron/src/Voltron/Prelude.hs"
