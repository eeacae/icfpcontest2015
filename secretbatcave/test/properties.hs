{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Property
import GHC.Arr (Ix(..))

import Batcave.Commands
import Batcave.Types
import Batcave.Hex

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

prop_emptyBoardNotOccupied :: BoardDims -> Property
prop_emptyBoardNotOccupied dims@(BoardDims w h) = forAll (validCell dims) $ \c ->
  (occupied (emptyBoard w h) c) === False

prop_cellCubicInvariance :: Cell -> Property
prop_cellCubicInvariance c = c === (cubicToCell . cellToCubic) c

-- SE == SW + E
prop_translateSE :: Unit -> Property
prop_translateSE u = translateUnitSouthEast u === (translateUnitSouthWest . translateUnitEast) u

-- SW == SE + W
prop_translateSW :: Unit -> Property
prop_translateSW u = translateUnitSouthWest u === (translateUnitSouthEast . translateUnitWest) u

prop_rotateInverse1 :: Unit -> Property
prop_rotateInverse1 u = u === (rotateUnitCW . rotateUnitCCW) u

prop_rotateInverse2 :: Unit -> Property
prop_rotateInverse2 u = u === (rotateUnitCCW . rotateUnitCW) u

prop_rotateOrder6CW :: Unit -> Property
prop_rotateOrder6CW u = u === (iterate rotateUnitCW u) !! 6

prop_rotateOrder6CCW :: Unit -> Property
prop_rotateOrder6CCW u = u === (iterate rotateUnitCCW u) !! 6

prop_translateCommutative :: Unit -> Gen Property
prop_translateCommutative u = do
  dir1 <- someTranslation
  dir2 <- someTranslation
  return $ (dir1 . dir2) u === (dir2 . dir1) u
  where
  someTranslation = elements [translateUnitWest, translateUnitEast, translateUnitSouthWest, translateUnitSouthEast]

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

prop_translateCellEastWestInverse :: Cell -> Property
prop_translateCellEastWestInverse c = c === (translateCellWest $ translateCellEast c)

prop_translateCellWestEastInverse :: Cell -> Property
prop_translateCellWestEastInverse c = c === (translateCellEast $ translateCellWest c)

prop_translateCellTriangleSouthEast :: Cell -> Property
prop_translateCellTriangleSouthEast c = translateCellSouthEast c === (translateCellSouthWest $ translateCellEast c)

prop_translateCellTriangleSouthWest :: Cell -> Property
prop_translateCellTriangleSouthWest c = translateCellSouthWest c === (translateCellSouthEast $ translateCellWest c)

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

return []
runTests = $quickCheckAll

main :: IO ()
main = do
  r <- runTests
  when (not r) $ exitWith (ExitFailure 1)
