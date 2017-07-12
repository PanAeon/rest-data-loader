{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import Lib
import qualified System.Directory as Dir
import Data.Foldable(forM_)
import Options.Applicative
import Data.Maybe(fromJust, fromMaybe)
import Data.List(isSuffixOf, stripPrefix, partition, intercalate)
import Data.Semigroup ((<>))
import System.IO(stderr, hPutStrLn)
import System.Exit
import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson as Aeson
import GHC.Generics
import Control.Monad(filterM, join)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Either.Utils(maybeToEither)
import Data.Foldable(find) -- bloody prelude
import Data.Either.Combinators(mapRight)
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

data ImportDefinition = ImportDefinition String Aeson.Value

data ImportDefinition' = ImportDefinition' String [String] Aeson.Value

-- FIXME: create Configuration class
-- FIXME: alternative, accumulating errors, applicative
dieHard:: String -> IO ()
dieHard msg = do
  hPutStrLn stderr msg
  exitFailure


data Node = Node String [Node] deriving Show

-- TODO "check for empty string in dependencies"
-- TODO "move noNothing up, and add directory to output"
buildDAG:: [(String, [String])] -> Either String [Node]
buildDAG xs = noNothing *> noRoots *> missingEither *> (sequence $ fmap (\x -> buildDAG' x [] rest) roots)
           where
             allNodes = Set.fromList $ fmap fst xs
             deps = Set.fromList . join $ fmap snd xs
             missingDeps = deps Set.\\ allNodes
             missingEither = if (null missingDeps) then
                                Right ()
                             else
                                Left $ "Missing dependencies: " ++ (intercalate ", " $ Set.toList missingDeps)
             (rest, roots) = partition (flip Set.member deps . fst)  xs
             noRoots :: Either String ()
             noRoots = if (null roots) then
                         mapRight (const ()) $ buildDAG' (head rest)  [] (tail rest)
                       else
                         Right ()
             noNothing = if (null xs) then Left "No import *.json files found in dir <dir>"
                                    else Right ()



buildDAG' :: (String, [String]) -> [String] -> [(String, [String])] -> Either String Node
buildDAG' root@(r, descendants) parents xs = let
                       rest = filter ((/=r) . fst) xs
                       descE = sequence $ fmap (\d -> maybeToEither ("Cyclic dependency with " ++ (intercalate "=>" (reverse parents)) ++ "=>" ++ r ++ "=>" ++ d) $ find ((==d) . fst) rest) descendants

                    in
                       do
                         desc <- descE
                         descNodesE <- sequence $ fmap (\d -> buildDAG' d (r : parents) rest) desc

                         return $ Node r descNodesE


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

-- FIXME: AESON parse errors are somehow stupid
parseData :: FilePath -> IO ()
parseData workDir = do
                    xs   <-  files
                    let ys = filter (isSuffixOf ".json") $ filter (/="manifest.json") xs
                    zs <- filterM (Dir.doesFileExist . ((workDir ++ "/")++)) ys
                --    let zzs = map ((workDir ++ "/")++) zs
                    mapM_ putStrLn zs
                    -- FIXME: arrows, collect errors (with applicative?)
                    ts <- sequence $ map (\p ->
                                    (readJSON $ toAbsolute p) >>= (\e ->
                                       return $ fmap (\v -> (stripSuffix ".json" p, v)) e
                                    )
                              ) zs -- FIXME: but you want not full path => json, but name => json !!
                    let tts = sequence ts
                    either dieHard (\rest ->
                          putStrLn $ show (Map.fromList rest)
                      ) tts

                    where
                      files = (Dir.listDirectory workDir)
                      toAbsolute = ((workDir ++ "/")++)
                      stripSuffix:: String -> String -> String
                      stripSuffix sfx lst =
                        fromMaybe lst $ fmap (reverse) $ stripPrefix (reverse sfx) (reverse lst)


parseData' workDir =  (Dir.listDirectory workDir)
       >>= ( mapM_ putStrLn)
