{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Applicative
import           Control.Concurrent
import           Options.Applicative

data Options = Options
    { limitTime   :: Maybe Int
    , limitMemory :: Maybe Int
    , limitCores  :: Maybe Int
    , problems    :: [FilePath]
    , phrases     :: [String]
    }
  deriving (Show)

parse :: Parser Options
parse = Options
        <$> option (Just <$> auto) ( short 't' <> metavar "T" <> help "Time limit" <> value Nothing)
        <*> option (Just <$> auto) ( short 'm' <> metavar "M" <> help "Memory limit" <> value Nothing)
        <*> option (Just <$> auto) ( short 'c' <> metavar "C" <> help "Cores" <> value Nothing)
        <*> some (option str (short 'f' <> metavar "FILE" <> help "Problem file"))
        <*> some (option str (short 'p' <> metavar "WORD" <> help "Word of power"))

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> parse)
           (fullDesc
           <> progDesc "Solve problems"
           <> header "solve - solve problems"
           )

run :: Options -> IO ()
run o@Options{..} = do
  threads <- maybe getNumCapabilities return limitCores
  print o
