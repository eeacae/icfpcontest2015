{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import           Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Array as Array
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.Vector as V
import           System.Environment (getArgs)
import           System.IO (stderr)

import           Batcave.RunGame (Game(..), ActiveUnit(..), GameEnd(..))
import           Batcave.RunGame (initGame, stepGame, gameScore, unGameScore)
import qualified Batcave.Solver.FloRida as FloRida
import qualified Batcave.Solver.Lucky as Lucky
import qualified Batcave.Solver.Nostrovia as Nostrovia
import           Batcave.Types

import           Debug.Trace

------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
      (solver:path:[]) -> do
        problem <- readProblem path
        let solution = solveWith solver problem
        L.putStrLn (A.encode (encodeFrames problem solution))
        L.hPutStrLn stderr (A.encode (V.singleton solution))

      _ -> putStrLn "Usage: dump SOLVER PROBLEM"
  where
    solveWith solver = case lookup solver solvers of
        Nothing -> error ("Unknown solver: " ++ show solver ++ ", valid solvers are: " ++ show (map fst solvers))
        Just s  -> s

    solvers = [ ("lucky",     Lucky.solve)
              , ("florida",   FloRida.solve)
              , ("nostrovia", Nostrovia.solve) ]

------------------------------------------------------------------------

encodeFrames :: Problem -> Solution -> A.Value
encodeFrames problem@Problem{..} solution@Solution{..} =
    A.object [ "width"  .= problemWidth
             , "height" .= problemHeight
             , "frames" .= frames ]
  where
    frames = V.map encodeGame
           $ V.unfoldr runStep (game0, commands)

    commands = V.fromList solutionCmds

    game0 = either (\msg -> error ("encodeFrames: " ++ show msg)) id (initGame problem solutionSeed)

    runStep (game1, cmds)
        | V.null cmds = Nothing
        | otherwise   = case stepGame (V.head cmds) game1 of
                          Left end -> traceShow end Nothing
                          Right  g -> Just (g, (g, V.tail cmds))

encodeGame :: Game -> A.Value
encodeGame game@Game{..} =
    A.object [ "locked"  .= fromBoard gameBoard
             , "current" .= fromMaybe V.empty (fromActive <$> gameActive)
             , "score"   .= takeScore game ]
  where
    fromBoard = V.fromList
              . mapMaybe full
              . Array.assocs
              . unBoard

    fromActive = V.map coord
               . V.fromList
               . Set.toList
               . unitMembers
               . activeUnit

    takeScore = unGameScore
              . gameScore

    full (cell, Full) = Just (coord cell)
    full _            = Nothing

    coord (Cell x y) = V.fromList [x, y]

------------------------------------------------------------------------

readProblem :: FilePath -> IO Problem
readProblem path = do
    bs <- L.readFile path
    return (either die id (A.eitherDecode bs))
  where
    die msg = error ("readProblem: failed reading json: " ++ msg)
