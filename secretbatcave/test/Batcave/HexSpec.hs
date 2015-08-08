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
import Batcave.Types
import Batcave.Hex

spec :: Spec
spec = describe "Tests for Batcave.Hex (board/grid coordinate logic)" $ do
  it "Empty board should be unoccupied" $ property $
    \dims@(BoardDims w h) ->
      forAll (validCell dims) $ \c ->
        (occupied (emptyBoard . fromJust $ boundingCells w h) c) === False
  it "Conversion in and out of cubic is idempotent" $ property $
    \c -> c === (cubicToCell . cellToCubic) c
  it "unit translation SE = SW + E" $ property prop_translateUnitTriangleSouthEast
  it "unit translation SW = SE + W" $ property prop_translateUnitTriangleSouthWest
  it "unit translation E + W = id" $ property prop_translateUnitEastWestInverse
  it "unit translation W + E = id" $ property prop_translateUnitWestEastInverse
  it "unit rotation CCW then CW = id" $ property $ prop_rotateUnitCCWCWInverse
  it "unit rotation CW then CCW = id" $ property $ prop_rotateUnitCWCCWInverse
  it "unit rotation CW 6 times = id" $ property prop_rotateOrder6CW
  it "unit rotation CCW 6 times = id" $ property prop_rotateOrder6CCW
  it "cell rotation CW/CCW is inverse" $ property prop_rotateCellCWCCWInverse
  it "cell rotation CW/CCW is inverse" $ property prop_rotateCellCCWCWInverse
  it "distance to pivot invariant under cell rotation " $
    property prop_rotateCellCWDistance
  it "distance to pivot invariant under cell rotation " $
    property prop_rotateCellCCWDistance
  it "cell distance is a metric space" $ property prop_cellDistanceNonNegative
  it "cell distance is a metric space" $ property prop_cellDistanceIdentity
  it "cell distance is a metric space" $ property prop_cellDistanceCommutative
  it "cell distance is a metric space" $ property prop_cellDistanceTriangle
  it "translation E = cell distance 1" $ property prop_translateCellEastDistance
  it "translation W = cell distance 1" $ property prop_translateCellWestDistance
  it "translation SE = cell distance 1" $ property prop_translateCellSouthEastDistance
  it "translation SW = cell distance 1" $ property prop_translateCellSouthWestDistance
  it "Movement commands commute" $ property prop_applyCommandCommutative
  it "clearBoard should remove a sensible number of cells" $
    property prop_clearBoardNumberOfCellsRemoved
  it "clearBoard should be idempotent" $
    property prop_clearBoardIdempotent
  it "clearBoard should empty full boards" $
    property prop_clearBoardWhenFull

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
  let full = Board $ array (unBounds b) [(c, Full) | c <- range (unBounds b)]
  return $ (emptyBoard b, h) === (clearBoard full)
