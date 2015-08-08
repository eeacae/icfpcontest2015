module Batcave.Hex where

import           Data.Array
import           Data.Char (toUpper)
import           Data.List (intersperse)
import qualified Data.Vector as V

import           Batcave.Commands
import           Batcave.Types

------------------------------------------------------------------------

-- | A hexagonal cubic coordinate
data Cubic = Cubic !Int !Int !Int

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

-- | Map over the cells in a unit.
mapUnit :: (Cell -> Cell) -> Unit -> Unit
mapUnit f u = u {
  unitMembers = fmap f $ unitMembers u,
  unitPivot   = f $ unitPivot u
}

-- | Convert a Cell to a Cubic. Taken from <http://www.redblobgames.com/grids/hexagons/>
cellToCubic :: Cell -> Cubic
cellToCubic (Cell col row) = Cubic x y z
  where x = col - (row - (abs row `mod` 2)) `div` 2
        z = row
        y = -x - z

-- | Convert a Cubic to a Cell.
cubicToCell :: Cubic -> Cell
cubicToCell (Cubic x y z) = Cell col row
  where col = x + (z - (abs z `mod` 2)) `div` 2
        row = z

-- | Translate a Cubic 1 space East
translateCubicEast :: Cubic -> Cubic
translateCubicEast (Cubic x y z) = Cubic (x + 1) (y - 1) z

-- | Translate a Cubic 1 space West
translateCubicWest :: Cubic -> Cubic
translateCubicWest (Cubic x y z) = Cubic (x - 1) (y + 1) z

-- | Translate a Cubic 1 space East
translateCubicSouthEast :: Cubic -> Cubic
translateCubicSouthEast (Cubic x y z) = Cubic x (y - 1) (z + 1)

-- | Translate a Cubic 1 space West
translateCubicSouthWest :: Cubic -> Cubic
translateCubicSouthWest (Cubic x y z) = Cubic (x - 1) y (z + 1)

-- | Rotate a Cubic vector clockwise
rotateCubicVectorCW :: Cubic -> Cubic
rotateCubicVectorCW (Cubic x y z) = Cubic (-z) (-x) (-y)

-- | Rotate a Cubic vector counter-clockwise
rotateCubicVectorCCW :: Cubic -> Cubic
rotateCubicVectorCCW (Cubic x y z) = Cubic (-y) (-z) (-x)

-- | Rotate a Cubic clockwise around a central Cubic
rotateCubicCW :: Cubic -> Cubic -> Cubic
rotateCubicCW centre@(Cubic x' y' z') cell@(Cubic x y z) = Cubic (x' + x'') (y' + y'') (z' + z'')
  where (Cubic x'' y'' z'') = rotateCubicVectorCW $ Cubic (x - x') (y - y') (z - z')

-- | Rotate a Cubic counter clockwise around a central Cubic
rotateCubicCCW :: Cubic -> Cubic -> Cubic
rotateCubicCCW centre@(Cubic x' y' z') cell@(Cubic x y z) = Cubic (x' + x'') (y' + y'') (z' + z'')
  where (Cubic x'' y'' z'') = rotateCubicVectorCCW $ Cubic (x - x') (y - y') (z - z')

-- | Perform operation on a Cell via a conversion to Cubic.
cubicOp :: (Cubic -> Cubic) -> Cell -> Cell
cubicOp f = cubicToCell . f . cellToCubic

-- | Translate a Cell 1 space East
translateCellEast :: Cell -> Cell
translateCellEast = cubicOp translateCubicEast

-- | Translate a Cell 1 space East
translateCellWest :: Cell -> Cell
translateCellWest = cubicOp translateCubicWest

-- | Translate a Cell 1 space East
translateCellSouthEast :: Cell -> Cell
translateCellSouthEast = cubicOp translateCubicSouthEast

-- | Translate a Cell 1 space East
translateCellSouthWest :: Cell -> Cell
translateCellSouthWest = cubicOp translateCubicSouthWest

-- | Rotate a Cell clockwise
rotateCellCW :: Cell -> Cell -> Cell
rotateCellCW centre cell = cubicOp (rotateCubicCW $ cellToCubic centre) cell

-- | Rotate a Cell counter clockwise
rotateCellCCW :: Cell -> Cell -> Cell
rotateCellCCW centre cell = cubicOp (rotateCubicCCW $ cellToCubic centre) cell

-- | Translate a Unit 1 space East
translateUnitEast :: Unit -> Unit
translateUnitEast = mapUnit translateCellEast

-- | Translate a Unit 1 space West
translateUnitWest :: Unit -> Unit
translateUnitWest = mapUnit translateCellWest

-- | Translate a Unit 1 space SouthEast
translateUnitSouthEast :: Unit -> Unit
translateUnitSouthEast = mapUnit translateCellSouthEast

-- | Translate a Unit 1 space SouthWest
translateUnitSouthWest :: Unit -> Unit
translateUnitSouthWest = mapUnit translateCellSouthWest

-- | Rotate a Unit clockwise
rotateUnitCW :: Unit -> Unit
rotateUnitCW u = mapUnit (rotateCellCW $ unitPivot u) u

-- | Rotate a Unit counter clockwise
rotateUnitCCW :: Unit -> Unit
rotateUnitCCW u = mapUnit (rotateCellCCW $ unitPivot u) u

applyCommand :: Command -> Unit -> Unit
applyCommand (Move E) = translateUnitEast
applyCommand (Move W) = translateUnitWest
applyCommand (Move SE) = translateUnitSouthEast
applyCommand (Move SW) = translateUnitSouthWest
applyCommand (Rotate Clockwise) = rotateUnitCW
applyCommand (Rotate CounterClockwise) = rotateUnitCCW

------------------------------------------------------------------------
-- Utilities to render the current game state

-- | Renders a RenderGrid as a string.
--
--   Empty cells are marked with "o", full ones with "x".
--   Cells that are part of the unit are marked with "u".
--   The pivot is marked by capitalizing the letter in the pivot cell.
--
--   eg. to try it out in ghci:
--     > import Batcave.Hex
--     > import Batcave.Types
--     > import qualified Data.Vector as V
--     > let u = Unit (V.fromList [Cell 1 0]) (Cell 0 0)
--     > let b = emptyBoard 5 3
--     > renderBoard 5 3 b u
--     O u o o o
--      o o o o o
--     o o o o o
instance Show RenderGrid where
  show (RenderGrid w h p c) =
    let cellChar x = case c ! x of
          RState Full  -> 'x'
          RState Empty -> 'o'
          RUnit        -> 'u'
        cellPChar x = if x == p then toUpper (cellChar x) else cellChar x
        row r       = map (flip Cell r) [ 0 .. (w-1) ]
        indent r    = if r `mod` 2 == 0 then "" else " "
        strRow r    = indent r ++ (intersperse ' ' $ map cellPChar (row r))
    in unlines $ map strRow [0 .. (h-1)]

-- | Grid to show the game state
data RenderGrid = RenderGrid {
    renderGridWidth  :: !Int
  , renderGridHeight :: !Int
  , renderGridPivot  :: !Cell
  , renderGridCells  :: Array Cell RenderCell
  }

-- | Status of a cell for rendering
data RenderCell = RState CellStatus  -- ^ full or empty cell, or...
                | RUnit              -- ^ part of the unit

-- | Produce a renderable grid of current board state
renderBoard :: Int         -- ^ width of board
            -> Int         -- ^ height of board
            -> Board       -- ^ board
            -> Unit        -- ^ unit
            -> RenderGrid  -- ^ simple renderable representation
renderBoard w h b u = RenderGrid
  { renderGridWidth  = w
  , renderGridHeight = h
  , renderGridPivot  = unitPivot u
  , renderGridCells  = listArray (bounds b) $ fmap (cellToRenderCell b u) (indices b)
  }

-- | Finds the value of a RenderCell given a board and unit.
cellToRenderCell :: Board       -- ^ board describing full / empty cells
                 -> Unit        -- ^ current unit
                 -> Cell        -- ^ cell coordinates
                 -> RenderCell  -- ^ render cell
cellToRenderCell b u c
  | inUnit    = RUnit
  | otherwise = RState occupy
  where occupy = b ! c
        inUnit = V.elem c (unitMembers u)
