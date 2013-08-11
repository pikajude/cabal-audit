module Audit.Options (
  Opts(..),
  fullOpts,
  module Options.Applicative.Extra
) where

import Options.Applicative
import Options.Applicative.Extra

data Opts = Opts { cabalFile :: Maybe FilePath } deriving Show

opts :: Parser Opts
opts = Opts <$> optional (argument str ( metavar "FILE"
                                      <> help ".cabal file to parse" ))

fullOpts :: ParserInfo Opts
fullOpts = info (helper <*> opts)
                ( fullDesc
               <> progDesc "Audit your .cabal file" )
