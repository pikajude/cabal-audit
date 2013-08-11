{-# LANGUAGE TupleSections #-}

module Audit (
  audit
) where

import Audit.Latest
import Audit.Panic
import Control.Monad
import Data.Maybe
import Data.Version
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Text
import Distribution.Verbosity
import Distribution.Version
import Text.Printf

audit :: FilePath -> IO ()
audit f = do
        pkgDesc <- readPackageDescription deafening f
        mapM_ (auditChunk id) (fmap ("library",) $ maybeToList $ condLibrary pkgDesc)
        mapM_ (auditChunk (printf "executable %s")) (condExecutables pkgDesc)
        mapM_ (auditChunk (printf "test suite %s")) (condTestSuites pkgDesc)
        mapM_ (auditChunk (printf "benchmark %s")) (condBenchmarks pkgDesc)

auditChunk :: (String -> String) -> (String, CondTree ConfVar [Dependency] a) -> IO ()
auditChunk f (e,m) = do
        putStrLn (f $ bold e)
        allOk <- mapM auditDep $ condTreeConstraints m
        when (and allOk) $ putStrLn $ success "ok!"

auditDep :: Dependency -> IO Bool
auditDep (Dependency (PackageName n) v)
    | n == "base" = return True
    | otherwise = do
        latest <- getLatestVersionOf n
        unless (withinRange latest v) $
            printf "%s\n  latest: %s\n  requested: %s\n" (warning n) (showVersion latest) (display v)
        return (withinRange latest v)
