{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
module Batcave.RunGame where

import Batcave.Types
import Batcave.Commands
import Batcave.Hex

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe
import qualified Data.Vector as V
import Data.Void

import Data.List

import qualified Data.Text as T

-- types to run the game

-- | overall game state
data Game = Game {
      source     :: [Unit] -- ^ all coming units, relative coordinates
    , board      :: Board -- Board state
    -- , history -- we probably want that at some point
    , cmds       :: [Command] -- ^ all remaining commands for the game
    , unitScores :: [UnitScore] -- ^ for final scoring, leftmost==newest
    }

data UnitScore = UnitScore {
      usSize  :: Int -- ^ size of the scoring unit
    , usLines :: Int -- ^ lines removed with this unit score 
    }

------------------------------------------------------------

data EndOfGame = OutOfUnits | OutOfCommands | CantPlaceUnit

runGame :: Board -> [Unit] -> [Command] -> [UnitScore]
runGame initial_board us cs =
  let
    initialGameState = Game {
      source = us
    , board = initial_board
    , cmds = cs
    , unitScores = []
    }
  in unitScores $
    flip execState initialGameState $ runExceptT runGameInternal

-- | Get the next unit from the queue.
popUnit :: (MonadState Game m, MonadError EndOfGame m) => m Unit
popUnit = do
  state <- get
  case source state of
    (u:us) -> do
      put $ state {source = us}
      return u
    [] -> throwError OutOfUnits

-- | Position the unit in the top centre.
-- Throw CantPlaceUnit if it's not placeable.
positionUnit :: (MonadState Game m, MonadError EndOfGame m) =>
  Unit -> m Unit
positionUnit = undefined
-- TODO: use a method in Batcave.Hex to implement this.

-- | Get the next command from the queue.
popCommand :: (MonadState Game m, MonadError EndOfGame m) => m Command
popCommand = do
  state <- get
  case cmds state of
    (c:cs) -> do
      put $ state {cmds = cs}
      return c
    [] -> throwError OutOfCommands

-- | Record the score data for the current unit.
pushUnitScore :: (MonadState Game m) => UnitScore -> m ()
pushUnitScore uScore =
    modify (\g -> g{unitScores = uScore : unitScores g})

-- | Runs the game within the monad until running out of commands/units.
runGameInternal :: (MonadState Game m, MonadError EndOfGame m) => m Void
runGameInternal = do
  u <- popUnit >>= positionUnit
  u' <- moveUntilLocked u
  lockPiece u'
  lines_scored <- clearLines
  pushUnitScore UnitScore {
    usSize = V.length $ unitMembers u,
    usLines = lines_scored
  }
  runGameInternal

-- | Locks a piece into place.
-- Precondition: unitPlaceable right now.
-- WARNING: pattern match fail otherwise!
lockPiece :: (MonadState Game m) => Unit -> m ()
lockPiece u = modify (mapBoard $ fromJust . placeUnit u)
  where mapBoard f gamestate = gamestate {board = f $ board gamestate}

-- | Scores any lines that are visible
-- Returns # lines scored.
clearLines :: (MonadState Game m) => m Int
clearLines = undefined
-- TODO: use a method in Batcave.Hex to implement this

-- | Moves a unit until it is locked in place.
moveUntilLocked :: (MonadState Game m, MonadError EndOfGame m) => Unit -> m Unit
moveUntilLocked unit = do
  -- precondition: unitPlaceable right now.
  b <- board <$> get
  c <- popCommand
  let unit' = applyCommand c unit
  if unitPlaceable b unit'
    then moveUntilLocked unit'
    else return unit
-- TODO: QuickCheck property: return value should be unitPlaceable 

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
moveScore :: (Int, Int)    -- ^ accumulator, last line count
              -> UnitScore  -- ^ current scoring 
              -> (Int, Int) -- ^ accumulator, lines counted
moveScore (acc, usLines_prev) UnitScore{..}
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
totalScore = fst . foldl' moveScore (0,0)

-- | score for a game (can be called any time during running it if
-- required) Note that unitScores are built right-to-left
currentGameScore :: Game -> Int
currentGameScore Game{..} = totalScore (reverse unitScores)

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
          counts = map (`T.count` cs') ps'
          -- This is inefficient (multiple traversals). Maybe OK?
