{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Shawty got low, low, low, low, low, low, low, low.
module Batcave.Solver.FloRida (solve, solveGame) where

import           Data.Function (on)
import           Data.List (maximumBy)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V

import           Batcave.BFS (validMoves)
import           Batcave.Commands
import           Batcave.RunGame (Game(..), initGame, runGame, ensureUnit)
import           Batcave.Types

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
    Nothing    -> []
    Just game1 -> let cmds  = nextMove game1
                      game2 = runGame cmds game1
                  in
                      maybe cmds (\g -> cmds ++ allMoves g) game2

nextMove :: Game -> [Command]
nextMove Game{..} = best
  where
    (_, best) = maximumBy (compare `on` unitTop . fst)
              $ validMoves unit gameBoard

    unit = fromMaybe (error msg) gameUnit
    msg  = "nextMove: invalid game state, must have a spawned unit!"


------------------------------------------------------------------------

unitTop :: Unit -> Int
unitTop = V.maximum . V.map top . unitMembers
  where
    top (Cell _ y) = y
