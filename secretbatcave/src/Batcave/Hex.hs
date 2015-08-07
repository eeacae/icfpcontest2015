module Batcave.Hex where

import Data.Array

-- | An initial game configuration and information on how many games to play.
data Game = Game {
  -- | A unique number identifying the problem
  id :: Int,
  {- |
   - The various unit configurations that may appear in this game.
   - There might be multiple entries for the same unit.
   - When a unit is spawned, it will start off in the orientation
   - specified in this field.
   -}
  units :: [Unit],
  -- | The number of cells in a row
  width :: Int,
  -- | The number of rows on the board
  height :: Int,
  -- | Which cells start filled
  filled:: [Cell],
  -- | How many units in the source
  sourceLength:: Int,
  -- | How to generate the source and how many games to play
  sourceSeeds:: [Int]
}

-- | The configuration of a unit. The cells are relative to a local coordinate
-- system, and will be translated to the board when the unit is spawned. The
-- local coordinate system of each cell, like the board's coordinate system,
-- has smaller row numbers in the "up" direction and smaller column numbers in
-- the "left" direction.
data Unit = Unit {
  -- | The unit members
  members :: [Cell],
  -- | The rotation point of the unit
  pivot :: Cell
}

-- | Identifies a cell, either on the board or within a unit.
type Cell = (Int, Int)

-- | Identifies a cell in terms of a hexagonal coordinate grid system.
-- (0, 0) corresponds to the board's (0, 0).
-- (e, se) denotes e steps east, then se steps southest, from the board's (0,0).
newtype AbsoluteCell = AbsoluteCell (Int, Int)

toAbs :: Cell -> AbsoluteCell
toAbs (x, y) = AbsoluteCell (x - (y `div` 2), y)

fromAbs :: AbsoluteCell -> Cell
fromAbs (AbsoluteCell (e, se)) = (e + (se `div` 2), se)

-- TODO: QuickCheck that toAbs and fromAbs are inverses.

-- | Whether a cell is full or empty.
data CellStatus = Full | Empty
  deriving (Eq, Ord, Show)

-- | A game board. The game board consists of hexagonal cells arranged in rows,
-- with the first row numbered 0. The cells in a row are oriented so that they
-- have vertices up and down, and edges to the left and right. The first cell
-- in a row is numbered 0, thus each cell may be identified by a pair of
-- coordinates (column, row).
type Board = Array Cell CellStatus

-- | An empty board of a width @w@ and height @h@.
emptyBoard :: Int -> Int -> Board
emptyBoard w h = array boundingCells [(c, Empty) | c <- range boundingCells]
  where boundingCells = ((0, 0), (w, h))

-- | Create the initial board described by a game configuration.
initialBoard :: Game -> Board
initialBoard g = emptyBoard (width g) (height g) // [(c, Full) | c <- filled g]

-- | Test whether a cell is within the bounds of a board.
inBounds :: Board -> Cell -> Bool
inBounds = inRange . bounds

-- | Test whether all of the members of a unit are within the bounds of a board.
unitInBounds :: Board -> Unit -> Bool
unitInBounds b u = all (inBounds b) $ members u

-- | Test whether a cell is occupied on a board.
occupied :: Board -> Cell -> Bool
occupied b c = b ! c == Full

-- | Test whether all of the members of a unit are unoccupied.
unitUnoccupied :: Board -> Unit -> Bool
unitUnoccupied b u = all (not . occupied b) $ members u

-- | Test whether a unit can be placed on a board.
unitPlaceable :: Board -> Unit -> Bool
unitPlaceable b u = unitInBounds b u && unitUnoccupied b u

-- | Place a unit on a board. Return @Nothing@ if the placement is invalid.
placeUnit :: Unit -> Board -> Maybe Board
placeUnit u b | unitPlaceable b u = Just $ b // [(c, Full) | c <- members u]
              | otherwise         = Nothing

-- | Translate the members of a unit by a given offset.
translateUnit :: Unit -> Cell -> Unit
translateUnit u (x', y') = u { members = [(x + x', y + y') | (x, y) <- members u]}
