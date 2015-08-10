{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Shawty got low, low, low, low, low, low, low, low.
module Batcave.Solver.FloRida (solve, solveGame) where

import           Data.Function (on)
import           Data.List (maximumBy)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V

import           Batcave.BFS (validMoves)
import           Batcave.Commands
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

    tag  = "flo-rida-"
        <> T.pack (show problemId)
        <> "-"
        <> T.pack (show (unSeed seed))

    game = either die id (initGame problem seed)

    die x = error ("FloRida.solve: " ++ show x)

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

nextMove :: Game -> [Command]
nextMove Game{..} = best
  where
    (_, best) = maximumBy (compare `on` unitSortOrder . fst)
              $ validMoves unit gameBoard

    unit = activeUnit (fromMaybe (error msg) gameActive)
    msg  = "nextMove: invalid game state, must have forgotten to spawn a unit!"


------------------------------------------------------------------------

-- | Return something that when maximised, selects the best unit placement.
unitSortOrder :: Unit -> Int
unitSortOrder unit = sum ys
  where
    ys = map cellY
       . Set.toList
       . unitMembers
       $ unit

    cellY (Cell _ y) = y
