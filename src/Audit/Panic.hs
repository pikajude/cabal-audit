module Audit.Panic where

import System.IO
import System.Exit
import Text.Printf

panic :: String -> IO a
panic s = do hPutStrLn stderr s
             exitFailure

worry :: String -> IO ()
worry = hPutStrLn stderr

bold :: String -> String
bold s = printf "\27[1m%s\27[0m" s

warning :: String -> String
warning s = printf "\27[33m%s\27[0m" s

success :: String -> String
success s = printf "\27[32m%s\27[0m" s
