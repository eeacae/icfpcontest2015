import Data.Array

data Game = Game {
  -- | A unique number identifying the problem
  id :: Int,
  {- |
   - The various unit configurations that may appear in this game.
   - There might be multiple entries for the same unit.
   - When a unit is spawned, it will start off in the orientation
   - specified in this field.
   -}
  units :: [Unit], 
  -- | The number of cells in a row
  width :: Int,
  -- | The number of rows on the board
  height :: Int,
  -- | Which cells start filled
  filled:: [Cell],
  -- | How many units in the source
  sourceLength:: Int,
  -- | How to generate the source and how many games to play
  sourceSeeds:: [Int]
}

data Unit = Unit {
  -- | The unit members
  members :: [Cell],
  -- | The rotation point of the unit
  pivot :: Cell
}

type Cell = (Int, Int)

data CellStatus = Full | Empty

type Board = Array Cell CellStatus
