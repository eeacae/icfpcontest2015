{-# LANGUAGE TemplateHaskell #-}

module Batcave.HexSpec where

import Data.Maybe
import Control.Monad
import Data.Array
import System.Exit
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property

import Batcave.Commands
import Batcave.Hex
import Batcave.Types

spec :: Spec
spec = describe "Tests for Batcave.Hex (board/grid coordinate logic)" $ do
  it "Empty board should be unoccupied" $ property $
    \dims@(BoardDims w h) ->
      forAll (validCell dims) $ \c ->
        (occupied (emptyBoard . fromJust $ boundingCells w h) c) === False
  it "Conversion in and out of cubic is idempotent" $ property $
    \c -> c === (cubicToCell . cellToCubic) c
  it "Unit translation SE = SW + E" $ property prop_translateUnitTriangleSouthEast
  it "Unit translation SW = SE + W" $ property prop_translateUnitTriangleSouthWest
  it "Unit translation E + W = id" $ property prop_translateUnitEastWestInverse
  it "Unit translation W + E = id" $ property prop_translateUnitWestEastInverse
  it "Unit rotation CCW then CW = id" $ property $ prop_rotateUnitCCWCWInverse
  it "Unit rotation CW then CCW = id" $ property $ prop_rotateUnitCWCCWInverse
  it "Unit rotation CW 6 times = id" $ property prop_rotateOrder6CW
  it "Unit rotation CCW 6 times = id" $ property prop_rotateOrder6CCW
  it "Cell rotation CW/CCW is inverse" $ property prop_rotateCellCWCCWInverse
  it "Cell rotation CW/CCW is inverse" $ property prop_rotateCellCCWCWInverse
  it "Distance to pivot invariant under cell rotation " $
    property prop_rotateCellCWDistance
  it "Distance to pivot invariant under cell rotation " $
    property prop_rotateCellCCWDistance
  it "Cell distance is a metric space" $ property prop_cellDistanceNonNegative
  it "Cell distance is a metric space" $ property prop_cellDistanceIdentity
  it "Cell distance is a metric space" $ property prop_cellDistanceCommutative
  it "Cell distance is a metric space" $ property prop_cellDistanceTriangle
  it "Translation E = cell distance 1" $ property prop_translateCellEastDistance
  it "Translation W = cell distance 1" $ property prop_translateCellWestDistance
  it "Translation SE = cell distance 1" $ property prop_translateCellSouthEastDistance
  it "Translation SW = cell distance 1" $ property prop_translateCellSouthWestDistance
  it "Movement commands commute" $ property prop_applyCommandCommutative
  it "clearBoard should remove a sensible number of cells" $ property prop_clearBoardNumberOfCellsRemoved
  it "clearBoard should be idempotent" $ property prop_clearBoardIdempotent
  it "Peak is an occupied cell" $ property prop_peakOccupied
  it "Cells higher than peak are unoccupied" $ property prop_peakHigherUnoccupied
  it "Column height is in valid range" $ property prop_columnHeightRange
  it "Column heights match board width" $ property prop_columnHeightsNumber
  it "Column heights are in valid range" $ property prop_columnHeightsRange
  it "Aggregate height is in valid range" $ property prop_aggregateHeightRange
  it "Length of complete rows is in valid range" $ property prop_completeRowsLength
  it "Complete rows are complete" $ property prop_completeRowsComplete
  it "Number of complete rows is in valid range" $ property prop_numCompleteRowsRange
  it "Length of column holes is in valid range" $ property prop_columnHolesLength
  it "Column holes are unoccupied" $ property prop_columnHolesUnoccupied
  it "Column holes are lower than column peaks" $ property prop_columnHolesNotPeaks
  it "Number of holes is in valid range" $ property prop_holesRange
  it "Bumpiness is in valid range" $ property prop_bumpinessRange
  it "Empty boards have 0 aggregate height" $ property prop_aggregateHeightEmptyBoard
  it "Empty boards have 0 complete rows" $ property prop_numCompleteRowsEmptyBoard
  it "Empty boards have 0 holes" $ property prop_holesEmptyBoard
  it "Empty boards have 0 bumpiness" $ property prop_bumpinessEmptyBoard

data BoardDims = BoardDims Int Int
  deriving (Eq, Show)

instance Arbitrary BoardDims where
  arbitrary = BoardDims <$> positiveInt <*> positiveInt

positiveInt :: Gen Int
positiveInt = arbitrary `suchThat` (> 0)

validCell :: BoardDims -> Gen Cell
validCell (BoardDims w h) = do
  x <- choose (0, w-1)
  y <- choose (0, h-1)
  pure $ Cell x y

prop_rotateOrder6CW :: Unit -> Property
prop_rotateOrder6CW u = u === (iterate rotateUnitCW u) !! 6

prop_rotateOrder6CCW :: Unit -> Property
prop_rotateOrder6CCW u = u === (iterate rotateUnitCCW u) !! 6

prop_cellDistanceNonNegative :: Cell -> Cell -> Property
prop_cellDistanceNonNegative c d = (cellDistance c d >= 0) === True

prop_cellDistanceIdentity :: Cell -> Property
prop_cellDistanceIdentity c = cellDistance c c === 0

prop_cellDistanceCommutative :: Cell -> Cell -> Property
prop_cellDistanceCommutative c d = cellDistance c d === cellDistance d c

prop_cellDistanceTriangle :: Cell -> Cell -> Cell -> Property
prop_cellDistanceTriangle c d e = (cellDistance c d + cellDistance d e >= cellDistance c e) === True

prop_translateCellEastDistance :: Cell -> Property
prop_translateCellEastDistance c = cellDistance c (translateCellEast c) === 1

prop_translateCellWestDistance :: Cell -> Property
prop_translateCellWestDistance c = cellDistance c (translateCellWest c) === 1

prop_translateCellSouthEastDistance :: Cell -> Property
prop_translateCellSouthEastDistance c = cellDistance c (translateCellSouthEast c) === 1

prop_translateCellSouthWestDistance :: Cell -> Property
prop_translateCellSouthWestDistance c = cellDistance c (translateCellSouthWest c) === 1

prop_rotateCellCWCCWInverse :: Cell -> Cell -> Property
prop_rotateCellCWCCWInverse c d = c === (rotateCellCCW d $ rotateCellCW d c)

prop_rotateCellCCWCWInverse :: Cell -> Cell -> Property
prop_rotateCellCCWCWInverse c d = c === (rotateCellCW d $ rotateCellCCW d c)

prop_rotateCellCWDistance :: Cell -> Cell -> Property
prop_rotateCellCWDistance c d = cellDistance c d === cellDistance c (rotateCellCW c d)

prop_rotateCellCCWDistance :: Cell -> Cell -> Property
prop_rotateCellCCWDistance c d = cellDistance c d === cellDistance c (rotateCellCCW c d)

prop_translateUnitEastWestInverse :: Unit -> Property
prop_translateUnitEastWestInverse u = u === (translateUnitWest $ translateUnitEast u)

prop_translateUnitWestEastInverse :: Unit -> Property
prop_translateUnitWestEastInverse u = u === (translateUnitEast $ translateUnitWest u)

prop_translateUnitTriangleSouthEast :: Unit -> Property
prop_translateUnitTriangleSouthEast u = translateUnitSouthEast u === (translateUnitSouthWest $ translateUnitEast u)

prop_translateUnitTriangleSouthWest :: Unit -> Property
prop_translateUnitTriangleSouthWest u = translateUnitSouthWest u === (translateUnitSouthEast $ translateUnitWest u)

prop_rotateUnitCWCCWInverse :: Unit -> Property
prop_rotateUnitCWCCWInverse u = u === (rotateUnitCCW $ rotateUnitCW u)

prop_rotateUnitCCWCWInverse :: Unit -> Property
prop_rotateUnitCCWCWInverse u = u === (rotateUnitCW $ rotateUnitCCW u)

prop_applyCommandCommutative :: Unit -> Command -> Command -> Property
prop_applyCommandCommutative u c1 c2 =
  ((applyCommand c1) . (applyCommand c2)) u ===
  ((applyCommand c2) . (applyCommand c1)) u

prop_clearBoardNumberOfCellsRemoved :: Board -> Property
prop_clearBoardNumberOfCellsRemoved (Board b) =
  nCells b === nCells b' + w * nLines
  where
  (w, _) = boardDimensions (Board b)
  nCells board = length $ filter (==Full) $ elems board
  (Board b', nLines) = clearBoard (Board b)

prop_clearBoardIdempotent :: Board -> Property
prop_clearBoardIdempotent b =
  afterClear b === (afterClear . afterClear) b
  where
  afterClear = fst . clearBoard

prop_clearBoardWhenFull :: Gen Property
prop_clearBoardWhenFull = do
  b <- arbitrary
  let (_, h) = boundDimensions b
  let full = Board $ array (takeBounds b) [(c, Full) | c <- range (takeBounds b)]
  return $ (emptyBoard b, h) === (clearBoard full)

prop_peakOccupied :: Board -> Int -> Property
prop_peakOccupied b c = maybe True (occupied b) p === True
  where p = columnPeak b c
        h = boardHeight b

prop_peakHigherUnoccupied :: Board -> Int -> Property
prop_peakHigherUnoccupied b c = maybe True higherUnoccupied p === True
  where p = columnPeak b c
        higherUnoccupied c' = all (not . occupied b) $ filter (\c'' -> cellHeight b c'' > cellHeight b c') $ boardColumn b c

prop_columnHeightRange :: Board -> Int -> Property
prop_columnHeightRange b c = (0 <= p && p <= h) === True
  where p = columnHeight b c
        h = boardHeight b

prop_columnHeightsNumber :: Board -> Property
prop_columnHeightsNumber b = boardWidth b === (length $ columnHeights b)

prop_columnHeightsRange :: Board -> Property
prop_columnHeightsRange b = (all (\p -> 0 <= p && p <= h) $ columnHeights b) === True
  where h = boardHeight b

prop_aggregateHeightRange :: Board -> Property
prop_aggregateHeightRange b = (0 <= a && a <= w * h) === True
  where (w, h) = boardDimensions b
        a = aggregateHeight b

prop_completeRowsLength :: Board -> Property
prop_completeRowsLength b = (0 <= n && n <= h) === True
  where h = boardHeight b
        n = length $ completeRows b

prop_completeRowsComplete :: Board -> Property
prop_completeRowsComplete b = (all (rowCompleted b) $ completeRows b) === True

prop_numCompleteRowsRange :: Board -> Property
prop_numCompleteRowsRange b = (0 <= n && n <= h) === True
  where h = boardHeight b
        n = numCompleteRows b

prop_columnHolesLength :: Board -> Int -> Property
prop_columnHolesLength b c = (0 <= n && n <= h) === True
  where h = boardHeight b
        n = length $ columnHoles b c

prop_columnHolesUnoccupied :: Board -> Int -> Property
prop_columnHolesUnoccupied b c = all (not . occupied b) hs === True
  where hs = columnHoles b c

prop_columnHolesNotPeaks :: Board -> Int -> Property
prop_columnHolesNotPeaks b c = all belowPeak hs === True
  where p = columnPeak b c
        hs = columnHoles b c
        belowPeak h = maybe False (\p' -> cellHeight b p' > cellHeight b h) p

prop_holesRange :: Board -> Property
prop_holesRange b = (0 <= a && a <= w * h) === True
  where (w, h) = boardDimensions b
        a = holes b

prop_bumpinessRange :: Board -> Property
prop_bumpinessRange b = (0 <= a && a <= w * h) === True
  where (w, h) = boardDimensions b
        a = bumpiness b

prop_aggregateHeightEmptyBoard :: BoardDims -> Property
prop_aggregateHeightEmptyBoard (BoardDims w h) =
        aggregateHeight (emptyBoard . fromJust $ boundingCells w h) === 0

prop_numCompleteRowsEmptyBoard :: BoardDims -> Property
prop_numCompleteRowsEmptyBoard (BoardDims w h) =
        numCompleteRows (emptyBoard . fromJust $ boundingCells w h) === 0

prop_holesEmptyBoard :: BoardDims -> Property
prop_holesEmptyBoard (BoardDims w h) =
        holes (emptyBoard . fromJust $ boundingCells w h) === 0

prop_bumpinessEmptyBoard :: BoardDims -> Property
prop_bumpinessEmptyBoard (BoardDims w h) =
        bumpiness (emptyBoard . fromJust $ boundingCells w h) === 0
