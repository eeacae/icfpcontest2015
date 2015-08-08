{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import System.Exit
import Test.QuickCheck
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

prop_translateCommutative :: Unit -> Gen Property
prop_translateCommutative u = do
  dir1 <- someTranslation
  dir2 <- someTranslation
  return $ (dir1 . dir2) u === (dir2 . dir1) u
  where
  someTranslation = elements [translateUnitWest, translateUnitEast, translateUnitSouthWest, translateUnitSouthEast]

return []
runTests = $quickCheckAll

main :: IO ()
main = do
  r <- runTests
  when (not r) $ exitWith (ExitFailure 1)
