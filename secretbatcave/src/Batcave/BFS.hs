-- | Breadth first search for locking moves
module Batcave.BFS
(
    allLockingPositions,
    bfs,
    singleton
) where

import           Batcave.Commands
import           Batcave.Hex
import           Batcave.Types
import           Data.Foldable
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Maybe       (listToMaybe, mapMaybe)
import           Data.Monoid
import           Data.Set         (Set)
import qualified Data.Set         as Set

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
allLockingPositions :: Unit -> Board -> ([Unit], Map Unit (Command, Unit))
allLockingPositions start = bfs (singleton start) mempty

-- | Return a list of the valid moves for the given unit on a board.
-- The valid moves contain the unit's final position on the board, and a list
-- of commands that will lock the unit in that position.
validMoves :: Unit -> Board -> [(Unit, [Command])]
validMoves u b = mapMaybe (backtrack ps) us
  where (us, ps) = bfs (singleton u) mempty b

-- | Find the final position of a unit and the list of commands that lead to
-- locking that unit in position.
backtrack :: Map Unit (Command, Unit) -> Unit -> Maybe (Unit, [Command])
backtrack ps u = fmap (\v -> (snd v, map fst $ reverse ucs)) $ listToMaybe ucs
  where ucs                 = backtrack' ps u
        backtrack' ps u     = maybe [] backtrack'' $ Map.lookup u ps
        backtrack'' (m, u') = (m, u') : backtrack' ps u'

-- | Unoptimised BFS over possible moves, accumulates moves that lock the unit,
-- queues up any valid move that has not already been visited.
bfs :: Queue Unit -> Map Unit (Command, Unit) -> Board -> ([Unit], Map Unit (Command, Unit))
bfs (Queue [] []) _ _ = mempty
bfs queue visited board =
    (Map.keys to_output, visited') <> bfs queue'' visited' board
  where
    (valid_children, invalid_children) = partitionChildren unit board
    to_queue = Map.difference valid_children visited
    to_output = Map.difference invalid_children visited
    visited' = Map.union valid_children $ Map.union invalid_children visited
    (unit, queue') = get queue
    queue'' = append queue' $ Map.keys to_queue

-- | All legal and illegal moves from the given position
partitionChildren :: Unit -> Board -> (Map Unit (Command, Unit), Map Unit (Command, Unit))
partitionChildren unit board =
    -- Find all legal moves from current unit
    let moves = zip (fmap (`applyCommand` unit) commands) $ zip commands $ repeat unit
        valid_moves = filter (unitPlaceable board . fst) moves
        invalid_moves = filter (not . unitPlaceable board .fst) moves
    in (Map.fromList valid_moves, Map.fromList invalid_moves)
  where
    commands =
        [ Move W
        , Move E
        , Move SW
        , Move SE
        , Rotate Clockwise
        , Rotate CounterClockwise
        ]