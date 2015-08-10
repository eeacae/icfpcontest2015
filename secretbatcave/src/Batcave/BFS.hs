{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

import Debug.Trace

------------------------------------------------------------------------
-- Queue

-- | Okasaki's, example for queues
data Queue a = Queue [a] [a]
    deriving (Show)

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

------------------------------------------------------------------------
-- Breadth First Search

data Path = Path {
      pathUnit    :: !Unit
    , pathHistory :: !(Set Unit)
    }

instance Show Path where
    showsPrec p Path{..} = showsPrec p pathUnit

instance Eq Path where
    (==) (Path x _) (Path y _) = x == y

instance Ord Path where
    compare (Path x _) (Path y _) = compare x y


-- | All possible last moves that lock a unit, none of these are valid moves
-- (they move onto a filled cell or off the board).
allLockingPositions :: Unit -> Board -> ([Unit], Map Path (Command, Unit))
allLockingPositions start = bfs (singleton path) (Map.singleton path (undefined, start))
  where
    path = Path start (Set.singleton start)

-- | Return a list of the valid moves for the given unit on a board.
-- The valid moves contain the unit's final position on the board, and a list
-- of commands that will lock the unit in that position.
validMoves :: Unit -> Board -> [(Unit, [Command])]
validMoves u b
    | unitPlaceable b u = if null moves
                          then error "validMoves: no valid moves"
                          else moves

    | otherwise         = error "validMoves: cannot start from a non-placeable unit"
  where
    (us, ps) = bfs (singleton path) (Map.singleton path (undefined, u)) b
    moves    = mapMaybe (backtrack ps) us
    path     = Path u (Set.singleton u)

-- | Find the final position of a unit and the list of commands that lead to
-- locking that unit in position.
backtrack :: Map Path (Command, Unit) -> Unit -> Maybe (Unit, [Command])
backtrack ps u = fmap (\v -> (snd v, reverse $ map fst ucs')) $ listToMaybe ucs
  where
    ucs  = backtrack' ps u
    ucs' = head ucs : (map snd $ takeWhile (\((_, u), (_, u')) -> u /= u') $ zip ucs $ tail ucs)

    -- NOTE: the Eq/Ord instance for Path only looks at the unit (not the history)
    backtrack' ps u     = maybe [] backtrack'' $ Map.lookup (Path u Set.empty) ps
    backtrack'' (m, u') = (m, u') : backtrack' ps u'

-- | Unoptimised BFS over possible moves, accumulates moves that lock the unit,
-- queues up any valid move that has not already been visited.
bfs :: Queue Path -> Map Path (Command, Unit) -> Board -> ([Unit], Map Path (Command, Unit))
bfs (Queue [] []) _ _     = mempty
bfs queue0 visited0 board =
    (map pathUnit (Map.keys to_output), visited1) <> bfs queue2 visited1 board
  where
    (valid_children, invalid_children) = partitionChildren path board

    to_queue       = valid_children   `Map.difference` visited0
    to_output      = invalid_children `Map.difference` visited0
    visited1       = Map.unions [visited0, valid_children, invalid_children]

    (path, queue1) = get queue0
    queue2         = append queue1 (Map.keys to_queue)

-- | All legal and illegal moves from the given position
partitionChildren :: Path
                  -> Board
                  -> (Map Path (Command, Unit),
                      Map Path (Command, Unit))

partitionChildren (Path unit0 hist0) board =
    -- Find all legal moves from current unit
    let
        apply cmd
            | legal     = Just (valid, (path1, (cmd, unit0)))
            | otherwise = Nothing
          where
            valid = all (unitPlaceable board) hist1
            legal = wontVisitHistory && wontLockUntilEnd

            wontVisitHistory = Set.null (Set.delete unit0 hist0 `Set.intersection` hist1)
            wontLockUntilEnd = all (unitPlaceable board) lock1

            applied = applyCommand cmd unit0
            unit1   = appUnit    applied
            hist1   = appHistory applied
            lock1   = appLock    applied
            path1   = Path unit1 (Set.union hist0 hist1)

        moves = mapMaybe apply commands

        (valid_moves, invalid_moves) = partition fst moves

    in (Map.fromList (map snd valid_moves),
        Map.fromList (map snd invalid_moves))
  where
    commands = reverse
        [ PhraseOfPower "ei!"
        , Move SW
        , Move SE
        , Move W
        , Move E
        , Rotate Clockwise
        , Rotate CounterClockwise
        ]
