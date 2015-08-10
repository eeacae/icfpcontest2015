{-# LANGUAGE OverloadedStrings #-}

-- | Breadth first search for locking moves
module Batcave.BFS
(
    validMoves,
    allLockingPositions,
    bfs,
    singleton
) where

import           Batcave.Commands
import           Batcave.Hex
import           Batcave.Types
import           Control.Arrow (first)
import           Data.Foldable
import           Data.List (partition)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (listToMaybe, mapMaybe)
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Okasaki's, example for queues
data Queue a = Queue [a] [a]

-- | Append to queue
put :: a -> Queue a -> Queue a
put item (Queue ins outs) = Queue (item:ins) outs

-- | Remove from queue
get :: Queue a -> (a, Queue a)
get (Queue ins (item:rest)) = (item, Queue ins rest)
get (Queue ins []) = get (Queue [] (reverse ins))

-- | Create a queue of one element
singleton :: a -> Queue a
singleton a = Queue [a] []

-- | Append many things
append :: Foldable f => Queue a -> f a -> Queue a
append queue to_queue = foldl' (flip put) queue to_queue

-- | All possible last moves that lock a unit, none of these are valid moves
-- (they move onto a filled cell or off the board).
allLockingPositions :: [T.Text] -> Unit -> Board -> ([Unit], Map Unit (Command, Unit))
allLockingPositions phrases start = bfs phrases (singleton start) (Map.singleton start (undefined, start))

-- | Return a list of the valid moves for the given unit on a board.
-- The valid moves contain the unit's final position on the board, and a list
-- of commands that will lock the unit in that position.
validMoves :: [T.Text] -> Unit -> Board -> [(Unit, [Command])]
validMoves phrases u b
    | unitPlaceable b u = mapMaybe (backtrack ps) us
    | otherwise         = error "validMoves: cannot start from a non-placeable unit"
  where
    (us, ps) = bfs phrases (singleton u) (Map.singleton u (undefined, u)) b

-- | Find the final position of a unit and the list of commands that lead to
-- locking that unit in position.
backtrack :: Map Unit (Command, Unit) -> Unit -> Maybe (Unit, [Command])
backtrack ps u = fmap (\v -> (snd v, reverse $ map fst ucs')) $ listToMaybe ucs
  where ucs                 = backtrack' ps u
        ucs'                = head ucs : (map snd $ takeWhile (\((_, u), (_, u')) -> u /= u') $ zip ucs $ tail ucs)
        backtrack' ps u     = maybe [] backtrack'' $ Map.lookup u ps
        backtrack'' (m, u') = (m, u') : backtrack' ps u'

-- | Unoptimised BFS over possible moves, accumulates moves that lock the unit,
-- queues up any valid move that has not already been visited.
bfs :: [T.Text] -> Queue Unit -> Map Unit (Command, Unit) -> Board -> ([Unit], Map Unit (Command, Unit))
bfs _ (Queue [] []) _ _ = mempty
bfs phrases queue visited board =
    (Map.keys to_output, visited') <> bfs phrases queue'' visited' board
  where
    (valid_children,
     invalid_children) = partitionChildren phrases unit board
    to_queue           = Map.difference valid_children visited
    to_output          = Map.difference invalid_children visited
    visited'           = Map.union visited $ Map.union valid_children invalid_children
    (unit, queue')     = get queue
    queue''            = append queue' $ Map.keys to_queue

-- | All legal and illegal moves from the given position
partitionChildren :: [T.Text] -> Unit -> Board -> (Map Unit (Command, Unit), Map Unit (Command, Unit))
partitionChildren phrases unit0 board =
    -- Find all legal moves from current unit
    let
        apply cmd = (valid, (unit1, (cmd, unit0)))
          where
            valid   = appPlaceable board applied
            unit1   = appUnit applied
            applied = applyCommand cmd unit0

        moves = map apply commands

        (valid_moves, invalid_moves) = partition fst moves

    in (Map.fromList (map snd valid_moves),
        Map.fromList (map snd invalid_moves))
  where
    commands = reverse $
        map PhraseOfPower phrases ++
        [ Move SW
        , Move SE
        , Move W
        , Move E
        , Rotate Clockwise
        , Rotate CounterClockwise
        ]
