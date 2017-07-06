module Main where

import Lib
import qualified System.Directory as Dir
import Data.Foldable(forM_)
import Options.Applicative
import Data.Semigroup ((<>))
import System.Exit
-- https://haskell-lang.org/library/http-client
-- https://hackage.haskell.org/package/hs-duktape
-- https://hackage.haskell.org/package/jsaddle
-- curl -s whatthecommit.com |  grep -A2 '<div id="content">' |  tail -n +2 | head -n +1 | sed 's/<p>//'
-- xinput --set-prop "FTE1001:00 0B05:0101" 'Device Enabled' $(xinput --list-props 'FTE1001:00 0B05:0101' | grep -c 'Device Enabled (139):[[:space:]].*0')
--import System.IO(FilePath)

workingDirectory::FilePath
workingDirectory = "/home/vitalii/lab/haskell/data"

createWorkingDirectoryIfMissing:: IO ()
createWorkingDirectoryIfMissing = Dir.createDirectoryIfMissing doCreateParents workingDirectory
                         where
                           doCreateParents = True



data ProgramArguments = ProgramArguments {
   workingDir :: FilePath
}

args :: Parser ProgramArguments
args = ProgramArguments
      <$>  strOption
          ( long "data-dir"
         <> short 'd'
         <> help "Data directory"
         <> showDefault
         <> value "/home/vitalii/lab/haskell/data"
         <> metavar "DATADIR" )

main :: IO ()
main = main' =<< execParser opts
       where
          opts = info (args <**> helper)
           ( fullDesc
             <> progDesc "Feed JSON documents to REST endpoint(s)"
             <> header "rest-data-loader - rest import tool" )

-- FIXME: use pathtype-0.8
checkWorkingDirectory :: FilePath -> IO Bool
checkWorkingDirectory = Dir.doesFileExist . (++"/manifest.json")

-- FIXME: _ifM ??? Control.Monad.Extra
main' :: ProgramArguments -> IO ()
main' (ProgramArguments workDir) = createWorkingDirectoryIfMissing >>
      checkWorkingDirectory workDir
      >>= (\manifestExists ->
        if (manifestExists) then
          doWork workDir
        else
          (putStrLn $ "Error: manifest.json doesn't exist in dir: '" ++ workDir ++ "'")
            >> (exitWith $ ExitFailure (-1))
      )
doWork workDir =  (Dir.listDirectory workDir)
       >>= ( mapM_ putStrLn)
