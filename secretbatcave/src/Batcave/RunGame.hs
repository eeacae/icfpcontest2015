{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
module Batcave.RunGame where

import Batcave.Types
import Batcave.Commands
import Batcave.Hex

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Vector as V

import Data.List

import qualified Data.Text as T

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

pushUnitScore :: (MonadState Game m) => UnitScore -> m ()
pushUnitScore = undefined

runGameInternal :: (MonadState Game m) => m ()
runGameInternal = do
  u <- movingUnit <$> get
  u' <- moveUntilLocked u
  lockPiece u'
  lines_scored <- clearLines
  pushUnitScore $ UnitScore {
    usSize = V.length $ unitMembers u,
    usLines = lines_scored
  }


-- | Locks a piece into place.
lockPiece :: (MonadState Game m) => Unit -> m ()
lockPiece u = modify undefined

-- | Scores any lines that are visible
-- Returns # lines scored.
clearLines :: (MonadState Game m) => m Int
clearLines = undefined

-- Moves a unit until it is locked in place.
moveUntilLocked :: (MonadState Game m) => Unit -> m Unit
moveUntilLocked unit = do
  b <- board <$> get
  c <- popCommand
  let unit' = move c unit
  if unitPlaceable b unit'
    then moveUntilLocked unit'
    else return unit
-- TODO: QuickCheck property: return value should be unitPlaceable 

-- | The effect of moving a unit, ignoring boundary conditions and
-- intersections.
-- TODO: placeholder, likely defined in Hex.
move :: Command -> Unit -> Unit
move = undefined

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
-}
move_score :: (Int, Int)    -- ^ accumulator, last line count
              -> UnitScore  -- ^ current scoring 
              -> (Int, Int) -- ^ accumulator, lines counted
move_score (acc, usLines_prev) UnitScore{..}
    = (acc + points + line_bonus, usLines)
  where points      = usSize
                         + 50 * (1 + ls) * ls
                      -- + 100 * (1 + ls) * ls / 2
        line_bonus  = if ls_old > 1
                      then floor ((ls_old - 1) * fromIntegral points / 10)
                      else 0
        ls_old      = fromIntegral usLines_prev
        ls          = fromIntegral usLines

-- | total score as a fold of a UnitScores list
totalScore :: [UnitScore] -> Int
totalScore = fst . foldl' move_score (0,0)

-- | score for a game (can be called any time during running it if
-- required
currentGameScore :: Game -> Int
currentGameScore Game{..} = totalScore unitScores

-- power phrase score for a sequence of commands
{-
 power_scorep = 2 * lenp * repsp + power_bonusp
  where
  power_bonusp = if repsp > 0
                 then 300
                 else 0
-}

-- | power score for a given count/repetitions
powerScore :: Int    -- ^ phrase length
               -> Int -- ^ repetitions
               -> Int
powerScore len reps = 2 * len * reps + power_bonus
  where power_bonus = if reps > 0
                      then 300
                      else 0

-- | converts commands to canonical text and phrases to canoncical
-- text, then counts and maps the score function over the counts)
scorePhrases :: [T.Text]   -- ^ list of phrases (18 at most)
             -> [Command]  -- ^ all commands
             -> Int        -- ^ total power score
scorePhrases ps cs = sum (zipWith powerScore (map T.length ps) counts)
    where cs'    = commandsToText cs
          ps'    = map canonicalizeCommand ps
          counts = map (\p -> T.count p cs') ps'
          -- This is inefficient (multiple traversals). Maybe OK?

