{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Batcave.Types
    ( Cell(..)

    , Unit
    , unitMembers
    , unitPivot
    , makeUnit
    , mapUnit
    , radixSort -- should probably be in another module

    , Seed(..)
    , Problem(..)
    , Solution(..)

    , CellStatus(..)

    , Bounds(..)
    , takeBounds
    , makeBounds
    , boundingCells
    , boundDimensions

    , Board(..)
    , emptyBoard
    , (%//)
    , (%!)
    , boardDimensions
    , boardBounds
    , inBounds
    ) where

import           Control.DeepSeq(NFData(..))
import           Control.Monad (mzero)
import           Control.Monad.ST (runST)
import           Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, (.:), (.=))
import           Data.Array
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Vector as V
import           Data.Vector.Algorithms.Radix (Radix(..))
import qualified Data.Vector.Algorithms.Radix as Radix
import           Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as U
import           Data.Vector.Unboxed.Deriving (derivingUnbox)
import           GHC.Arr (Ix (..))

import           Batcave.Commands
import           Test.QuickCheck

------------------------------------------------------------------------

-- | Identifies a cell, either on the board or within a unit.
data Cell = Cell !Int !Int
  deriving (Eq, Ord, Show)


-- Due to Template Haskell staging limitations, this has to be before any
-- unboxed uses of Cell.
$(derivingUnbox "Cell"
    [t| Cell -> (Int, Int) |]
    [| \(Cell x y) -> (x, y) |]
    [| \(x, y) -> (Cell x y) |])


------------------------------------------------------------------------

-- | The configuration of a unit.
--
--   The cells are relative to a local coordinate system, and will be translated
--   to the board when the unit is spawned. The local coordinate system of each
--   cell, like the board's coordinate system, has smaller row numbers in the
--   "up" direction and smaller column numbers in the "left" direction.
--
--   Note that the pivot cell does not have to be a member of the unit.
--
data Unit = Unit {

    -- | The unit members.
    unitMembers :: !(Set Cell)

    -- | The rotation point of the unit.
  , unitPivot   :: !Cell

  } deriving (Eq, Ord, Show)


-- | Map over the cells in a unit.
makeUnit :: Set Cell -> Cell -> Unit
makeUnit members pivot = Unit members pivot
{-# INLINE makeUnit #-}

-- | Map over the cells in a unit.
mapUnit :: (Cell -> Cell) -> Unit -> Unit
mapUnit f (Unit ms p) = makeUnit (Set.map f ms) (f p)
{-# INLINE mapUnit #-}

-- | Perform a radix sort of an unboxed vector.
radixSort :: (Unbox a, Radix a) => U.Vector a -> U.Vector a
radixSort unsorted = runST $ do
    thawed <- U.thaw unsorted
    Radix.sort thawed
    U.unsafeFreeze thawed
{-# INLINE radixSort #-}


------------------------------------------------------------------------

-- | Problem/solution seeds.
newtype Seed = Seed { unSeed :: Int }
  deriving (Eq, Ord, Show)


------------------------------------------------------------------------

-- | The game configuration.
data Problem = Problem {

    -- | A unique number identifying the problem.
    problemId           :: !Int

    -- | The various unit configurations that may appear in this game.
    --   There might be multiple entries for the same unit.
    --
    --   When a unit is spawned, it will start off in the orientation
    --   specified in this field.
    --
  , problemUnits        :: !(V.Vector Unit)

    -- | The number of cells in a row.
  , problemWidth        :: !Int

    -- | The number of rows on the board.
  , problemHeight       :: !Int

    -- | Which cells start filled.
  , problemFilled       :: !(Set Cell)

    -- | How many units in the source.
  , problemSourceLength :: !Int

    -- | How to generate the source and how many games to play.
  , problemSourceSeeds  :: !(V.Vector Seed)

  } deriving (Eq, Ord, Show)


------------------------------------------------------------------------

-- | A solution to a particular problem case.
data Solution = Solution {
      solutionProb :: !Int
    , solutionSeed :: !Seed
    , solutionTag  :: !(Maybe Text)
    , solutionCmds :: ![Command]
    } deriving (Show, Eq)


------------------------------------------------------------------------

-- | Whether a cell is full or empty.
data CellStatus = Full | Empty
  deriving (Eq, Ord, Show)
instance Arbitrary CellStatus where arbitrary = elements [Full, Empty]

data Bounds = Bounds !Cell !Cell
  deriving (Eq, Show)

takeBounds :: Bounds -> (Cell, Cell)
takeBounds (Bounds lo hi) = (lo, hi)

makeBounds :: (Cell, Cell) -> Bounds
makeBounds (lo, hi) = Bounds lo hi

boundingCells :: Int -> Int -> Maybe Bounds
boundingCells width height
  | width > 0 && height > 0 = Just $ Bounds (Cell 0 0) (Cell (width-1) (height-1))
  | otherwise               = Nothing

boundDimensions :: Bounds -> (Int, Int)
boundDimensions (Bounds _ (Cell w h)) = (w+1, h+1)

-- | A game board.
--
--   The game board consists of hexagonal cells arranged in rows, with the
--   first row numbered 0. The cells in a row are oriented so that they have
--   vertices up and down, and edges to the left and right. The first cell in a
--   row is numbered 0, thus each cell may be identified by a pair of
--   coordinates (column, row).
newtype Board = Board { unBoard :: Array Cell CellStatus }
  deriving (Eq, Show)

-- | An empty board of a width @w@ and height @h@.
emptyBoard :: Bounds -> Board
emptyBoard b = Board $ array (takeBounds b) [(c, Empty) | c <- range (takeBounds b)]

(%//) :: Board -> [(Cell, CellStatus)] -> Board
(%//) (Board b) cs = Board $ b // cs

(%!) :: Board -> Cell -> CellStatus
(%!) (Board b) c = b ! c

-- | Get the width and height of a board.
boardDimensions :: Board -> (Int, Int)
boardDimensions (Board b) = (w + 1, h + 1)
  where (_, Cell w h) = bounds b

-- | Get the bounds of a board.
boardBounds :: Board -> Bounds
boardBounds = makeBounds . bounds . unBoard

-- | Test whether a cell is within the bounds of a board.
inBounds :: Board -> Cell -> Bool
inBounds = inRange . bounds . unBoard


instance NFData Solution where
    rnf Solution{..} = rnf solutionCmds `seq`
                       rnf solutionTag  `seq`
                       solutionProb + unSeed solutionSeed `seq` ()

------------------------------------------------------------------------
-- Aeson Instances

instance FromJSON Cell where
    parseJSON (Object o) = Cell <$> o .: "x"
                                <*> o .: "y"
    parseJSON _          = mzero

instance FromJSON Unit where
    parseJSON (Object o) = Unit <$> o .: "members"
                                <*> o .: "pivot"
    parseJSON _          = mzero

instance FromJSON Problem where
    parseJSON (Object o) = Problem <$> o .: "id"
                                   <*> o .: "units"
                                   <*> o .: "width"
                                   <*> o .: "height"
                                   <*> o .: "filled"
                                   <*> o .: "sourceLength"
                                   <*> (V.map Seed <$> o .: "sourceSeeds")
    parseJSON _          = mzero

instance ToJSON Solution where
    toJSON Solution{..} = object $
                      [ "problemId" .= solutionProb
                      , "seed"      .= unSeed solutionSeed
                      , "solution"  .= solutionCmds
                      ] <> maybe [] (\v -> ["tag" .= v]) solutionTag

------------------------------------------------------------------------
-- Ix instances

instance Ix Cell where
    {-# INLINE range #-}
    range (Cell l1 l2, Cell u1 u2) =
      [ Cell i1 i2
      | i1 <- range (l1, u1)
      , i2 <- range (l2, u2) ]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (Cell l1 l2, Cell u1 u2) (Cell i1 i2) =
        unsafeIndex     (l1, u1) i1
      * unsafeRangeSize (l2, u2)
      + unsafeIndex     (l2, u2) i2

    {-# INLINE inRange #-}
    inRange (Cell l1 l2, Cell u1 u2) (Cell i1 i2) =
         inRange (l1, u1) i1
      && inRange (l2, u2) i2


------------------------------------------------------------------------
-- Radix instances

instance Radix Cell where
    {-# INLINE passes #-}
    passes _ = passes (undefined :: Int) * 2

    {-# INLINE size #-}
    size   _ = size (undefined :: Int)

    -- NOTE: This will sort such that y has priority over x.
    --       This ordering is by default more useful to solvers.
    {-# INLINE radix #-}
    radix k (Cell x y) | k < half  = radix k x
                       | otherwise = radix (k - half) y
      where
        half = passes (undefined :: Int)


------------------------------------------------------------------------
-- Arbitrary instances

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary

instance (Arbitrary a, Unbox a)  => Arbitrary (U.Vector a) where
  arbitrary = U.fromList <$> arbitrary

instance (Arbitrary a, Ord a)  => Arbitrary (Set a) where
  arbitrary = Set.fromList <$> arbitrary

instance Arbitrary Bounds where
  arbitrary = (Bounds (Cell 0 0))
    <$> (suchThat arbitrary $ \(Cell hc hr) ->
      hc >= 0 && hr >= 0)

instance Arbitrary Board where
  arbitrary = do
    (Bounds l h) <- arbitrary
    elems <- mapM (\c -> (c,) <$> arbitrary) $ range (l, h)
    pure . Board $ array (l, h) elems

instance Arbitrary Cell where
  arbitrary = Cell <$> arbitrary <*> arbitrary

instance Arbitrary Unit where
  arbitrary = Unit <$> arbitrary <*> arbitrary
