{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module X.Language.Haskell.Exts.CabalUtils ( findExtensions ) where

import Control.Lens                                 ( view )
import Data.Set                                     ( fromList )
import Distribution.PackageDescription.Parse        ( ParseResult( ParseFailed
                                                                 , ParseOk
                                                                 )
                                                    , parseGenericPackageDescription
                                                    )
import Distribution.Types.BuildInfo                 ( defaultExtensions
                                                    , otherExtensions
                                                    )
import Distribution.Types.GenericPackageDescription ( packageDescription )
import Distribution.Types.PackageDescription        ( allBuildInfo )
import Language.Haskell.Extension                   ( Extension )
import Protolude
import Refactorio.Conversions                       ( convert )

-- TODO: wire up to read cabal files from targets when available
-- TODO: test to confirm this is equivalent to the more complicated longboye version
findExtensions :: ByteString -> [Extension]
findExtensions bs = case parseGenericPackageDescription . view convert $ bs of
  ParseFailed e -> panic $ "findExtensions failed: " <> show e
  ParseOk _warnings packDesc  -> allExts
    where
      buildInfos = allBuildInfo . packageDescription $ packDesc
      allExts    = toList . fromList $ dexts ++ oexts
      dexts      = join $ map defaultExtensions buildInfos
      oexts      = join $ map otherExtensions buildInfos
