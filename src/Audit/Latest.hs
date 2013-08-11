module Audit.Latest (
  getLatestVersionOf
) where

import Audit.Panic
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.ParseUtils
import Distribution.Version
import Network.HTTP
import Text.Printf

getLatestVersionOf :: String -> IO Version
getLatestVersionOf pkg = do
        resp <- simpleHTTP (getRequest url)
        case resp of
            Left err -> panic $ "Connection error when connecting to hackage: " ++ show err
            Right s -> do
                let desc = parsePackageDescription $ rspBody s
                case desc of
                    ParseFailed pe -> giveUp pe
                    ParseOk ws d -> do
                        mapM_ (worry . showPWarning (pkg ++ ".cabal")) ws
                        return . pkgVersion . package $ packageDescription d
    where url = printf "http://new-hackage.haskell.org/package/%s/src/%s.cabal" pkg pkg
          giveUp pe = case ln of
                          Nothing -> panic $ printf "Error when parsing %s.cabal: %s" pkg s
                          Just num -> panic $ printf "Error when parsing %s.cabal: line %d: %s" pkg num s
              where (ln, s) = locatedErrorMsg pe
