{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Parallel.Strategies
import           Control.DeepSeq
import           Control.Exception
import           System.IO -- buffering
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
    , useSolver   :: String
    , phrases     :: [String]
    , problems    :: [FilePath]
    }
  deriving (Show)

parse :: Parser Options
parse = Options
        <$> option (Just <$> auto) ( short 't' <> metavar "T" <> help "Time limit" <> value Nothing)
        <*> option (Just <$> auto) ( short 'm' <> metavar "M" <> help "Memory limit" <> value Nothing)
        <*> option (Just <$> auto) ( short 'c' <> metavar "C" <> help "Cores" <> value Nothing)
        <*> strOption              ( short 's' <> metavar "SOLVER" <> help "Solver" <> value "FloRida")
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

    -- run concurrent/parallel threads for each problem, using a channel to
    -- capture and write results as they come in
    solutions <- newChan

    let solveTask t = do s <- evaluate $ force (solve ["ei!"] t)
                         writeChan solutions s

    ids <- mapM (forkIO . solveTask) (catMaybes ps)
    
    -- main thread reads results (lazily from channel) and adds them to output
    hSetBuffering stdout LineBuffering
    outputAll =<< concat . take (length ids) <$> getChanContents solutions

  where
    solve :: [String] -> Problem -> [Solution]
    -- Must add power words when rendering json (we do not have a
    -- string here). Equivalent replacements in command sequence,
    -- starting with the shortest word, such that longer ones are used
    -- where possible.
    solve powerPhrases = 
        -- solver interface function defined in Batcave.Solvers.solvers
      fromMaybe useFloRida (lookup useSolver solvers) powerPhrases
        -- useFloRida   (default)
        -- useNostrovia

    outputAll :: [Solution] -> IO ()
    outputAll s =
        let go [] = return ()
            go (r:rs) = BS.putStrLn (encode r <> ",") >> go rs
        in putStrLn "[" >> go s >> putStrLn "]"

