{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import System.Exit
import Test.QuickCheck
import GHC.Arr (Ix(..))

import Batcave.Types
import Batcave.Hex

data BoardDims = BoardDims Int Int
  deriving (Eq, Show)

instance Arbitrary BoardDims where
  arbitrary = BoardDims <$> positiveInt <*> positiveInt

positiveInt :: Gen Int
positiveInt = arbitrary `suchThat` (> 0)

validCell :: BoardDims -> Gen Cell
validCell (BoardDims w h) = suchThat arbitrary (inRange (Cell 0 0, Cell (w-1) (h-1)))

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

return []
runTests = $quickCheckAll

main :: IO ()
main = do
  r <- runTests
  when (not r) $ exitWith (ExitFailure 1)
