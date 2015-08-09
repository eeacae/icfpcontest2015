-- | Breadth first search for locking moves
module Batcave.BFS
(
    allLockingPositions,
    singleton,
    bfs
) where

import           Batcave.Commands
import           Batcave.Hex
import           Batcave.Types
import           Data.Foldable
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
append :: Foldable f => f a -> Queue a -> Queue a
append to_queue queue = foldl' (flip put) queue to_queue

-- | All possible last moves that lock a unit, none of these are valid moves
-- (they move onto a filled cell or off the board).
allLockingPositions :: Unit -> Board -> Set Unit
allLockingPositions start = bfs (singleton start) mempty

-- | Unoptimised BFS over possible moves, accumulates moves that lock the unit,
-- queues up any valid move that has not already been visited.
bfs :: Queue Unit -> Set Unit -> Board -> Set Unit
bfs (Queue [] []) _ _ = mempty
bfs queue visited board =
    to_output <> bfs queue'' visited' board
  where
    (valid_children, invalid_children) = partitionChildren unit board
    to_queue = Set.difference valid_children visited
    to_output = Set.difference invalid_children visited
    visited' = Set.union valid_children $ Set.union invalid_children visited
    (unit, queue') = get queue
    queue'' = append to_queue queue'

-- | All legal and illegal moves from the given position
partitionChildren :: Unit -> Board -> (Set Unit, Set Unit)
partitionChildren unit board =
    -- Find all legal moves from current unit
    let moves = fmap (`applyCommand` unit) commands
        valid_moves = filter (unitPlaceable board) moves
        invalid_moves = filter (not . unitPlaceable board) moves
    in (Set.fromList valid_moves, Set.fromList invalid_moves)
  where
    commands =
        [ Move W
        , Move E
        , Move SW
        , Move SE
        , Rotate Clockwise
        , Rotate CounterClockwise
        ]
