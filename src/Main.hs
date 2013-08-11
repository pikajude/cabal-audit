import Audit
import Audit.Options
import Audit.Panic
import Data.List
import System.Directory
import System.FilePath

locateCabalFiles :: Opts -> IO [FilePath]
locateCabalFiles (Opts cf) = case cf of Nothing -> guess
                                        Just f -> return [f]
    where guess = do cont <- getDirectoryContents "."
                     return $ filter (\f -> takeExtension f == ".cabal") cont

main :: IO ()
main = do opts <- execParser fullOpts
          file <- locateCabalFiles opts
          case file of [f] -> audit f
                       [] -> panic "No .cabal file found! Please specify one using -f FILE"
                       xs -> panic $ "Multiple cabal files found: " ++ intercalate ", " xs ++ ". Please specify one using -f FILE"
