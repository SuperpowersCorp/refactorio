{-# LANGUAGE NoImplicitPrelude #-}

module SrcLocsSpec where

import           Refactorio.Prelude

import           Control.Lens
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as C8
import           Data.Data.Lens
import           Test.Hspec                           hiding ( after
                                                             , before
                                                             )
import           X.Language.Haskell.Exts                     ( Comment
                                                             , Module
                                                             , SrcSpanInfo
                                                             , exactPrint
                                                             )
import           X.Language.Haskell.Exts.Prisms

spec :: Spec
spec = describe "Trying to figure out srcLoc updating..." $ do
  code <- runIO $ BS.readFile "test/FixtureActual.hs"
  exp1 <- runIO $ BS.readFile "test/FixtureExpected1.hs"
  let parsed = fromMaybe (panic "unpossible!") $ code ^? _Hask

  context "I" $ do

    it "should be able to update srcLocs correctly after a modification" $ do
      let updated  = parsed & (_1 . biplate . _Ident . _2 . filtered (== "x")) .~ "xxx"
          adjusted = adjustSrcLocs parsed updated
      uncurry exactPrint adjusted `shouldBe` C8.unpack exp1
  where
    adjustSrcLocs :: (Module SrcSpanInfo, [Comment])
                  -> (Module SrcSpanInfo, [Comment])
                  -> (Module SrcSpanInfo, [Comment])
    adjustSrcLocs _before after = after
