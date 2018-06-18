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

-- CURRENT TARGET:   refio --haskell view "__Module.biplate._Int" & "(+32)"

process :: Config -> IO ()
process Config{..} = build (unExpression expr) >>= either (panic . show) process'
  where
    process' f = do
      putLn $ "Seeking " <> show (unExpression expr)
      S.mapM_ ( processWith f )
        . S.filter ( matchesFilters filenameFilters )
        . S.filter ( not . ignored )
        . S.map fst
        . S.filter ( not . isDirectory . snd )
        . tree
        . unTarget
        $ target

matchesFilters :: Set FilenameFilter -> FilePath -> Bool
matchesFilters filters path
  | Set.null filters = True
  | otherwise        = any (flip matches path) filters

-- TODO: read .*ignore files from the target dir down to the current file, caching
--       along the way, etc. but for now...
ignored :: FilePath -> Bool
ignored = (".stack-work" `List.isInfixOf`)

processWith :: (ByteString -> ByteString) -> FilePath -> IO ()
processWith f path = do
  bytes :: ByteString <- BS.readFile path
  let bytes' :: ByteString = f bytes
  if (bytes == bytes')
    then putLn $ "** Unchanged: " <> show path
    else do
      let xs  :: [ByteString] = C8.lines bytes
          xs' :: [ByteString] = C8.lines bytes'
          diff'               = getContextDiff ctxLines xs xs'
          doc                 = prettyContextDiff d1name d2name elpp diff'
      putLn $ "** Changed: " <> show path
      putLn "================================================================"
      putLn . render' $ doc
  where
    d1name :: Doc
    d1name = PP.text "BEFORE"

    d2name :: Doc
    d2name = PP.text "AFTER"

    ctxLines :: Int
    ctxLines = 2

    elpp :: ByteString -> Doc
    elpp = PP.text . unpack . decodeUtf8

    render' :: Doc -> Text
    render' = pack . PP.render

_test1 :: IO ()
_test1 = do
  putLn "== test1 start =="
  processWith BS.reverse path
  putLn "== test1 complete =="
  where
    path = "/tmp/voltron/src/Voltron/Prelude.hs"
