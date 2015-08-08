module Batcave.RunGame where

import Batcave.Types
import Batcave.Commands
import Batcave.Hex

-- types to run the game

-- | overall game state
data Game = Game {
      source     :: [Unit] -- ^ all coming units, relative coordinates
    , movingUnit :: Unit -- ^ Absolute coordinates
    -- , history -- we probably want that at some point
    , cmds       :: [Command] -- ^ all remaining commands for the game
    , unitScores :: [UnitScore] -- ^ for final scoring
    }

data UnitScore = UnitScore {
      usSize  :: Int -- ^ size of the scoring unit
    , usLines :: Int -- ^ lines removed with this unit score 
    }

------------------------------------------------------------
-- moving pieces as a state monad TODO





------------------------------------------------------------
-- scoring

-- given per-unitscore scores
{-
move_score = points + line_bonus
  where
  points = size + 100 * (1 + ls) * ls / 2
  line_bonus  = if ls_old > 1
                then floor ((ls_old - 1) * points / 10)
                else 0

move_score :: (Int, Int) -- ^ accumulator, last line count
              -> 
move_score 

= points + line_bonus
  where
  points = size + 100 * (1 + ls) * ls / 2
  line_bonus  = if ls_old > 1
                then floor ((ls_old - 1) * points / 10)
                else 0

move_score = points + line_bonus
  where
  points = size + 100 * (1 + ls) * ls / 2
  line_bonus  = if ls_old > 1
                then floor ((ls_old - 1) * points / 10)
                else 0

-- | total score as a fold of a UnitScores list
-}
