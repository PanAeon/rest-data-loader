module RestDataLoader.ArgumentParser (
          parseArguments
        , parseArguments'
) where

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad.Reader

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

parseArguments' :: (ProgramArguments -> IO ()) -> IO ()
parseArguments' rest = rest =<< execParser opts
       where
          opts = info (args <**> helper)
           ( fullDesc
             <> progDesc "Feed JSON documents to REST endpoint(s)"
             <> header "rest-data-loader - rest import tool" )

parseArguments :: Reader ProgramArguments (IO ()) -> IO ()
parseArguments r = (runReader r) =<< execParser opts
       where
          opts = info (args <**> helper)
           ( fullDesc
             <> progDesc "Feed JSON documents to REST endpoint(s)"
             <> header "rest-data-loader - rest import tool" )
