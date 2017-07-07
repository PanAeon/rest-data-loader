{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import Lib
import qualified System.Directory as Dir
import Data.Foldable(forM_)
import Options.Applicative
import Data.List(isSuffixOf, stripPrefix)
import Data.Semigroup ((<>))
import System.Exit
import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson as Aeson
import GHC.Generics
import Control.Monad(filterM)
import qualified Data.Map as Map
-- FIXME: I believe I had find out some sort of immutable hashtable or trie

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


readJSON :: FilePath -> IO (Either String Aeson.Value)
readJSON path =  fmap Aeson.eitherDecode (B.readFile path)

data Manifest = Manifest {
         _host :: String,
         _port :: Int
} deriving (Show, Generic)

instance Aeson.FromJSON Manifest
instance Aeson.ToJSON Manifest

-- FIXME: create Configuration class

-- FIXME: _ifM ??? Control.Monad.Extra
main' :: ProgramArguments -> IO ()
main' (ProgramArguments workDir) = createWorkingDirectoryIfMissing >>
      checkWorkingDirectory workDir
      >>= (\manifestExists ->
        if (manifestExists) then
          parseData workDir
        else
          (putStrLn $ "Error: manifest.json doesn't exist in dir: '" ++ workDir ++ "'")
            >> (exitWith $ ExitFailure (-1))
      )

parseData :: FilePath -> IO ()
parseData workDir = do
                    xs   <-  files
                    let ys = filter (isSuffixOf ".json") $ filter (/="manifest.json") xs
                    zs <- filterM (Dir.doesFileExist . ((workDir ++ "/")++)) ys
                --    let zzs = map ((workDir ++ "/")++) zs
                    mapM_ putStrLn zs
                    -- FIXME: arrows, collect errors (with applicative?)
                    ts <- sequence $ map (\p ->
                                    (readJSON $ toAbsolute p) >>= (\e -> return (stripSuffix ".json" p, e))
                              ) zs -- FIXME: but you want not full path => json, but name => json !!
                    let m = Map.fromList ts
                    putStrLn $ show m
                    --return ()
                    -- haleluya
                    where
                      files = (Dir.listDirectory workDir)
                      toAbsolute = ((workDir ++ "/")++)
                      stripSuffix sfx lst =
                        case stripPrefix (reverse sfx) (reverse lst) of
                            Nothing -> Nothing
                            Just ys -> Just (reverse ys)

parseData' workDir =  (Dir.listDirectory workDir)
       >>= ( mapM_ putStrLn)
