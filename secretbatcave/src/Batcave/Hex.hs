{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Batcave.Hex where

import           Data.Array
import           Data.Char (toUpper)
import           Data.List (intersperse, maximumBy)
import           Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Batcave.Commands
import           Batcave.Types

------------------------------------------------------------------------

-- | A hexagonal cubic coordinate
data Cubic = Cubic !Int !Int !Int

-- | Create the initial board described by a game configuration.
initialBoard :: Problem -> Maybe Board
initialBoard p = (flip (%//) filled . emptyBoard) <$> limits
 where 
  limits = boundingCells (problemWidth p) (problemHeight p)
  filled = [(c, Full) | c <- Set.toList (problemFilled p)]

-- | Clear a row from a board.
clearRow :: Int -> Board -> Board
clearRow r b = b %// ([(Cell x y, b %! (Cell x (y - 1))) | x <- xs, y <- ys] ++ [(Cell x 0, Empty) | x <- xs])
  where (w, _) = boardDimensions b
        xs     = [0..w - 1]
        ys     = [1..r]

-- | Get the column of a cell
cellColumn :: Cell -> Int
cellColumn (Cell x _) = x

-- | Get the column of a cell
cellRow :: Cell -> Int
cellRow (Cell _ y) = y

-- | Calculate the height of a cell in a board
cellHeight :: Board -> Cell -> Int
cellHeight b c = boardHeight b - cellRow c

-- | Get the width of a board.
boardWidth :: Board -> Int
boardWidth = fst . boardDimensions

-- | Get the height of a board.
boardHeight :: Board -> Int
boardHeight = snd . boardDimensions

-- | Get a row from a board.
boardRow :: Board -> Int -> [Cell]
boardRow b r | 0 <= r && r < h  = [Cell x r | x <- [0..w - 1]]
             | otherwise        = []
  where (w, h) = boardDimensions b 

-- | Get a column from a board.
boardColumn :: Board -> Int -> [Cell]
boardColumn b c | 0 <= c && c < w  = [Cell c y | y <- [0..h - 1]]
                | otherwise        = []
  where (w, h) = boardDimensions b 

-- | Test whether a row in a board is completed.
rowCompleted :: Board -> Int -> Bool
rowCompleted b r = all (occupied b) [Cell x r | x <- [0..w - 1]]
  where (w, _)   = boardDimensions b

-- | Clear completed rows from a board.
clearBoard :: Board -> (Board, Int)
clearBoard b = (b', length rs)
  where (_, h) = boardDimensions b
        rs     = filter (rowCompleted b) [h - 1, h - 2..0]
        b'     = foldr clearRow b rs

-- | Test whether all of the members of a unit are within the bounds of a board.
unitInBounds :: Board -> Unit -> Bool
unitInBounds b u = all (inBounds b) $ unitMembers u

-- | Test whether a cell is occupied on a board.
occupied :: Board -> Cell -> Bool
occupied b c = b %! c == Full

-- | Test whether all of the members of a unit are unoccupied.
unitUnoccupied :: Board -> Unit -> Bool
unitUnoccupied b u = all (not . occupied b) $ unitMembers u

-- | Test whether a unit can be placed on a board.
unitPlaceable :: Board -> Unit -> Bool
unitPlaceable b u = unitInBounds b u && unitUnoccupied b u

-- | Place a unit on a board. Return @Nothing@ if the placement is invalid.
placeUnit :: (Unit, Unit) -> Board -> Maybe Board
placeUnit (u, u') b | unitPlaceable b u' = Just $ b %// [(c, Full) | c <- Set.toList (unitMembers u)]
                    | otherwise          = Nothing

-- | Move a unit to the location it would be spawned on the given board.
spawnUnit :: Unit -> Board -> Maybe Unit
spawnUnit u b
    | unitPlaceable b spawned = Just spawned
    | otherwise               = Nothing
  where
    spawned = mapUnit shift u

    shift (Cell x y) = Cell (x + xShift) (y + yShift)

    xShift = bxMid - uxMid
    yShift = -uyMin

    bxMid = bxMin + (bxMax - bxMin) `div` 2
    uxMid = uxMin + (uxMax - uxMin) `div` 2

    Bounds (Cell bxMin     _) (Cell bxMax _) = boardBounds b
    Bounds (Cell uxMin uyMin) (Cell uxMax _) = unitBounds u

-- | Find the bounds of a unit.
unitBounds :: Unit -> Bounds
unitBounds unit
    | Set.null (unitMembers unit) = error "unitBounds: unit had no members"
    | otherwise                   = Bounds unitMin unitMax
  where
    unitMin = Set.foldl' (merge min) (Cell maxBound maxBound) (unitMembers unit)
    unitMax = Set.foldl' (merge max) (Cell minBound minBound) (unitMembers unit)

    merge op (Cell x0 y0) (Cell x1 y1) = Cell (op x0 x1) (op y0 y1)

-- | Convert a Cell to a Cubic. Taken from <http://www.redblobgames.com/grids/hexagons/>
cellToCubic :: Cell -> Cubic
cellToCubic (Cell c r) = Cubic x y z
  where x = c - (r - (abs r `mod` 2)) `div` 2
        z = r
        y = -x - z

-- | Convert a Cubic to a Cell.
cubicToCell :: Cubic -> Cell
cubicToCell (Cubic x _ z) = Cell c r
  where c = x + (z - (abs z `mod` 2)) `div` 2
        r = z

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
rotateCubicCW (Cubic x' y' z') (Cubic x y z) = Cubic (x' + x'') (y' + y'') (z' + z'')
  where (Cubic x'' y'' z'') = rotateCubicVectorCW $ Cubic (x - x') (y - y') (z - z')

-- | Rotate a Cubic counter clockwise around a central Cubic
rotateCubicCCW :: Cubic -> Cubic -> Cubic
rotateCubicCCW (Cubic x' y' z') (Cubic x y z) = Cubic (x' + x'') (y' + y'') (z' + z'')
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

-- fst is the actual resulting unit to be placed.
-- snd is a faux-unit which represents all the spots
-- that need to be clear in order to execute this
-- move.
applyCommand :: Command -> Unit -> (Unit, Unit)
applyCommand (Move E)  u = let m = translateUnitEast      u in (m,m)
applyCommand (Move W)  u = let m = translateUnitWest      u in (m,m)
applyCommand (Move SE) u = let m = translateUnitSouthEast u in (m,m)
applyCommand (Move SW) u = let m = translateUnitSouthWest u in (m,m)
applyCommand (Rotate Clockwise)        u = let m = rotateUnitCW  u in (m,m)
applyCommand (Rotate CounterClockwise) u = let m = rotateUnitCCW u in (m,m)
applyCommand (PowerWord s) u = T.foldl f (u,u) s
    where
        f (u', u'') c = let (nu', nu'') = applyCommand (charToCommand c) u'
                        in (nu', u'' `mergeUnit` nu'')
        mergeUnit u1 u2 = makeUnit (unitMembers u1 `Set.union` unitMembers u2) (unitPivot u1)

-- | Find the highest occupied cell in a column of a board
columnPeak :: Board -> Int -> Maybe Cell
columnPeak b c | null ocs  = Nothing
               | otherwise = Just $ maximumBy (comparing (cellHeight b)) ocs
  where ocs = filter (occupied b) $ boardColumn b c

-- | Find the height of a column in a board
columnHeight :: Board -> Int -> Int
columnHeight b c = maybe 0 (cellHeight b) $ columnPeak b c

-- | Calculate the height of each column
columnHeights :: Board -> [Int]
columnHeights b = map (columnHeight b) $ [0..w - 1]
  where w = boardWidth b

-- | Calculate the aggregate height of the board
aggregateHeight :: Board -> Int
aggregateHeight = sum . columnHeights

-- | Find the completed rows in the board
completeRows :: Board -> [Int]
completeRows b = filter (rowCompleted b) [h - 1, h - 2..0]
  where h = boardHeight b

-- | Calculate the number of complete rows in the board
numCompleteRows :: Board -> Int
numCompleteRows = length . completeRows

-- | Find the holes in a column of the board
columnHoles :: Board -> Int -> [Cell]
columnHoles b c = filter isHole $ boardColumn b c
  where isHole d = not (occupied b d) && cellHeight b d < columnHeight b (cellColumn d)

-- | Calculate the number of holes in the board
holes :: Board -> Int
holes b = sum $ map (length . columnHoles b) $ [0..w-1]
  where w = boardWidth b

-- | Calculate the bumpiness of the top of the board
bumpiness :: Board -> Int
bumpiness b = sum $ map (abs . uncurry (-)) $ zip hs (tail $ hs)
  where hs = columnHeights b

-- | Calculate heuristic on board
heuristic :: Double -> Double -> Double -> Double -> Board -> Double
heuristic u v w x b =
  u * (fromIntegral $ aggregateHeight b) +
  v * (fromIntegral $ numCompleteRows b) +
  w * (fromIntegral $ holes b) +
  x * (fromIntegral $ bumpiness b)

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
renderBoard w h (Board b) u = RenderGrid
  { renderGridWidth  = w
  , renderGridHeight = h
  , renderGridPivot  = unitPivot u
  , renderGridCells  = listArray (bounds b) $ fmap (cellToRenderCell (Board b) u) (indices b)
  }

-- | Finds the value of a RenderCell given a board and unit.
cellToRenderCell :: Board       -- ^ board describing full / empty cells
                 -> Unit        -- ^ current unit
                 -> Cell        -- ^ cell coordinates
                 -> RenderCell  -- ^ render cell
cellToRenderCell b u c
  | inUnit    = RUnit
  | otherwise = RState occupy
  where occupy = b %! c
        inUnit = Set.member c (unitMembers u)
