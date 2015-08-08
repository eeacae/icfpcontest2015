{-# LANGUAGE FlexibleContexts #-}

module Batcave.RunGame where

import Batcave.Types
import Batcave.Commands
import Batcave.Hex
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State

-- types to run the game

-- | overall game state
data Game = Game {
      source     :: [Unit] -- ^ all coming units, relative coordinates
    , movingUnit :: Unit -- ^ Absolute coordinates
    , board      :: Board -- Board state
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

data EndOfGame = OutOfUnits | OutOfCommands

runGame :: [Unit] -> [Command] -> [UnitScore]
runGame = undefined

popUnit :: (MonadState Game m) => m Unit
popUnit = undefined

popCommand :: (MonadState Game m) => m Command
popCommand = undefined

runGameInternal :: (MonadState Game m) => m ()
runGameInternal = do
  u <- movingUnit <$> get
  u' <- moveUntilLocked u
  scoreBoard u'

-- | Locks a piece into place and then scores any lines
scoreBoard :: (MonadState Game m) => Unit -> m ()
scoreBoard = undefined

-- Moves a unit until it is locked in place.
moveUntilLocked :: (MonadState Game m) => Unit -> m Unit
moveUntilLocked = undefined

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
