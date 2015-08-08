module Batcave.Hex where

import           Data.Array
import qualified Data.Vector as V

import           Batcave.Commands
import           Batcave.Types

------------------------------------------------------------------------

-- | A hexagonal cubic coordinate
data Cubic = Cubic !Int !Int !Int

-- | An empty board of a width @w@ and height @h@.
emptyBoard :: Int -> Int -> Board
emptyBoard w h = array boundingCells [(c, Empty) | c <- range boundingCells]
  where boundingCells = (Cell 0 0, Cell (w - 1) (h - 1))

-- | Create the initial board described by a game configuration.
initialBoard :: Problem -> Board
initialBoard p = emptyBoard (problemWidth p) (problemHeight p) // [(c, Full) | c <- V.toList (problemFilled p)]

-- | Get the width and height of a board.
boardDimensions :: Board -> (Int, Int)
boardDimensions b = (w + 1, h + 1)
  where (_, Cell w h) = bounds b

-- | Clear a row from a board.
clearRow :: Int -> Board -> Board
clearRow r b = b // ([(Cell x y, b ! (Cell x (y - 1))) | x <- xs, y <- ys] ++ [(Cell x 0, Empty) | x <- xs])
  where (w, h) = boardDimensions b
        xs     = [0..w - 1]
        ys     = [1..r]

-- | Test whether a row in a board is completed.
rowCompleted :: Board -> Int -> Bool
rowCompleted b r = all (occupied b) [Cell x r | x <- [0..w - 1]]
  where (w, h)   = boardDimensions b

-- | Clear completed rows from a board.
clearBoard :: Board -> (Board, Int)
clearBoard b = (b', length rs)
  where (w, h) = boardDimensions b
        rs     = filter (rowCompleted b) [h - 1, h - 2..0]
        b'     = foldr clearRow b rs

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

-- | Place a unit on a board. Return @Nothing@ if the placement is invalid.
placeUnit :: Unit -> Board -> Maybe Board
placeUnit u b | unitPlaceable b u = Just $ b // [(c, Full) | c <- V.toList (unitMembers u)]
              | otherwise         = Nothing

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

-- | Calculate the distance between Cubics
cubicDistance :: Cubic -> Cubic -> Int
cubicDistance (Cubic x y z) (Cubic x' y' z') =
  (abs (x - x') + abs (y - y') + abs (z - z')) `div` 2

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

-- | Calculate the distance between Cells
cellDistance :: Cell -> Cell -> Int
cellDistance c d = cubicDistance (cellToCubic c) (cellToCubic d)

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
