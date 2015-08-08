{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Batcave.Types where

import           Control.Applicative
import           Control.Monad       (mzero)
import           Data.Aeson          (FromJSON (..), ToJSON (..), Value (..),
                                      object, (.:), (.=))
import qualified Data.Vector as V
import           Data.Array          (Array, array)
import           Data.Monoid
import           Data.Vector         (Vector)
import           GHC.Arr             (Ix (..))

import           Batcave.Commands
import           Test.QuickCheck
------------------------------------------------------------------------

-- | Identifies a cell, either on the board or within a unit.
data Cell = Cell !Int !Int
  deriving (Eq, Ord, Show)


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
    unitMembers :: !(Vector Cell)

    -- | The rotation point of the unit.
  , unitPivot   :: !Cell

  } deriving (Eq, Ord, Show)


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
  , problemUnits        :: !(Vector Unit)

    -- | The number of cells in a row.
  , problemWidth        :: !Int

    -- | The number of rows on the board.
  , problemHeight       :: !Int

    -- | Which cells start filled.
  , problemFilled       :: !(Vector Cell)

    -- | How many units in the source.
  , problemSourceLength :: !Int

    -- | How to generate the source and how many games to play.
  , problemSourceSeeds  :: !(Vector Int)

  } deriving (Eq, Ord, Show)


------------------------------------------------------------------------

-- | Whether a cell is full or empty.
data CellStatus = Full | Empty
  deriving (Eq, Ord, Show)


-- | A game board.
--
--   The game board consists of hexagonal cells arranged in rows, with the
--   first row numbered 0. The cells in a row are oriented so that they have
--   vertices up and down, and edges to the left and right. The first cell in a
--   row is numbered 0, thus each cell may be identified by a pair of
--   coordinates (column, row).
type Board = Array Cell CellStatus

-- | A solution to a particular problem case.
data Solution = Solution
    { solutionProb :: !Int
    , solutionSeed :: !Int
    , solutionTag  :: Maybe String
    , solutionCmds :: [Command]
    }
  deriving (Show, Eq)

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
                                   <*> o .: "sourceSeeds"
    parseJSON _          = mzero

instance ToJSON Solution where
    toJSON Solution{..} = object $
                      [ "problemId" .= solutionProb
                      , "seed" .= solutionSeed
                      , "solution" .= solutionCmds
                      ] <> maybe [] (\v -> ["tag" .= v]) solutionTag

------------------------------------------------------------------------
-- Ix Instances

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
-- Arbitrary instances

instance (Arbitrary a => Arbitrary (Vector a)) where
  arbitrary = V.fromList <$> arbitrary

instance (Arbitrary e) => Arbitrary (Array Cell e) where
  arbitrary = do
    (l, Cell hc hr) <- boardDims
    elems <- arbitrary `suchThat` all (inRange (l, (Cell (hc-1) (hr-1))) . fst)
    pure $ array (l, Cell hc hr) elems
    where
      boardDims :: Gen (Cell, Cell)
      boardDims = suchThat arbitrary $ \(Cell lc lr, Cell hc hr) ->
        hc >= lc && hr >= lr && lc >= 0 && lr >= 0

instance Arbitrary Cell where
  arbitrary = Cell <$> arbitrary <*> arbitrary

instance Arbitrary Unit where
  arbitrary = Unit <$> arbitrary <*> arbitrary
