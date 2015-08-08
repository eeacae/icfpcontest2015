module Batcave.Hex where

import           Data.Array
import qualified Data.Vector as V

import           Batcave.Types

------------------------------------------------------------------------

-- | Identifies a cell in terms of a hexagonal coordinate grid system.
-- (0, 0) corresponds to the board's (0, 0).
-- (e, se) denotes e steps east, then se steps southest, from the board's (0,0).
data AbsoluteCell = AbsoluteCell !Int !Int

toAbs :: Cell -> AbsoluteCell
toAbs (Cell x y) = AbsoluteCell (x - (y `div` 2)) y

fromAbs :: AbsoluteCell -> Cell
fromAbs (AbsoluteCell e se) = Cell (e + (se `div` 2)) se

-- TODO: QuickCheck that toAbs and fromAbs are inverses.

-- | An empty board of a width @w@ and height @h@.
emptyBoard :: Int -> Int -> Board
emptyBoard w h = array boundingCells [(c, Empty) | c <- range boundingCells]
  where boundingCells = (Cell 0 0, Cell w h)

-- | Create the initial board described by a game configuration.
initialBoard :: Problem -> Board
initialBoard p = emptyBoard (problemWidth p) (problemHeight p) // [(c, Full) | c <- V.toList (problemFilled p)]

-- | Test whether a cell is within the bounds of a board.
inBounds :: Board -> Cell -> Bool
inBounds = inRange . bounds

-- | Test whether all of the members of a unit are within the bounds of a board.
unitInBounds :: Board -> Unit -> Bool
unitInBounds b u = all (inBounds b) $ unitMembers u

-- | Test whether a cell is occupied on a board.
occupied :: Board -> Cell -> Bool
occupied b c = b ! c == Full

-- | Test whether all of the members of a unit are unoccupied.
unitUnoccupied :: Board -> Unit -> Bool
unitUnoccupied b u = all (not . occupied b) $ unitMembers u

-- | Test whether a unit can be placed on a board.
unitPlaceable :: Board -> Unit -> Bool
unitPlaceable b u = unitInBounds b u && unitUnoccupied b u

-- | Place a unit on a board. Return @Nothing@ if the placement is invalid.
placeUnit :: Unit -> Board -> Maybe Board
placeUnit u b | unitPlaceable b u = Just $ b // [(c, Full) | c <- V.toList (unitMembers u)]
              | otherwise         = Nothing
