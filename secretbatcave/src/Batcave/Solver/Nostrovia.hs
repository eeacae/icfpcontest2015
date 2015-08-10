{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Batcave.Solver.Nostrovia (solve, solveGame) where

import           Data.Function (on)
import           Data.List (maximumBy)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V

import           Batcave.BFS (validMoves)
import           Batcave.Commands
import           Batcave.Hex
import           Batcave.RunGame (Game(..), ActiveUnit(..), initGame, runGame, ensureUnit)
import           Batcave.Types

import           Debug.Trace

------------------------------------------------------------------------

-- interface
solveGame :: Game -> [Command]
solveGame = allMoves

solve :: Problem -> Solution
solve problem@Problem{..} = Solution {
      solutionProb = problemId
    , solutionSeed = seed
    , solutionTag  = Just tag
    , solutionCmds = allMoves game
    }
  where
    seed = V.head problemSourceSeeds

    tag  = "nostrovia-"
        <> T.pack (show problemId)
        <> "-"
        <> T.pack (show (unSeed seed))

    game = either die id (initGame problem seed)

    die x = error ("Nostrovia.solve: " ++ show x)

------------------------------------------------------------------------

allMoves :: Game -> [Command]
allMoves game0 =
  case ensureUnit game0 of
    Left  end   -> traceShow end []
    Right game1 -> let cmds  = nextMove game1
                       game2 = runGame cmds game1
                   in
                       either (\e -> traceShow e cmds)
                              (\g -> cmds ++ allMoves g) game2

myHeuristic :: Board -> Unit -> Double
myHeuristic b u = maybe (error msg) (heuristic (-0.510066) 0.760666 (-0.35663) (-0.184483)) (placeUnit (u,u) b)
  where
    msg = "myHeuristic: received non-placeable unit!"

nextMove :: Game -> [Command]
nextMove Game{..} = best
  where
    (_, best) = maximumBy (compare `on` myHeuristic gameBoard . fst)
              $ validMoves unit gameBoard

    unit = activeUnit (fromMaybe (error msg) gameActive)
    msg  = "nextMove: invalid game state, must have a spawned unit!"
