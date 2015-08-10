{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Parallel.Strategies
import           Data.Aeson
import qualified Data.ByteString.Lazy        as BS
import           Data.List
import           Data.Maybe
import           Options.Applicative

import           Batcave.IO
import           Batcave.Types
import           Batcave.Solvers

data Options = Options
    { limitTime   :: Maybe Int
    , limitMemory :: Maybe Int
    , limitCores  :: Maybe Int
    , phrases     :: [String]
    , problems    :: [FilePath]
    }
  deriving (Show)

parse :: Parser Options
parse = Options
        <$> option (Just <$> auto) ( short 't' <> metavar "T" <> help "Time limit" <> value Nothing)
        <*> option (Just <$> auto) ( short 'm' <> metavar "M" <> help "Memory limit" <> value Nothing)
        <*> option (Just <$> auto) ( short 'c' <> metavar "C" <> help "Cores" <> value Nothing)
        <*> many (option str (short 'p' <> metavar "WORD" <> help "Word of power"))
        <*> some (option str (short 'f' <> metavar "FILE" <> help "Problem file"))

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
    ps <- mapM loadProblemFile problems
    outputAll . concat $ parMap rdeepseq (solve phrases) (catMaybes ps)
  where
    solve :: [String] -> Problem -> [Solution]
    -- Must add power words when rendering json (we do not have a
    -- string here). Equivalent replacements in command sequence,
    -- starting with the shortest word, such that longer ones are used
    -- where possible.
    solve powerPhrases = 
        -- solver interface function defined in Batcave.Solvers:
        useFloRida powerPhrases
        -- useNostrovia

    outputAll :: [Solution] -> IO ()
    outputAll s =
        let go [] = return ()
            go (r:rs) = BS.putStrLn (encode r <> ",") >> go rs
        in putStrLn "[" >> go s >> putStrLn "]"

