{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Batcave.RunGame
    ( Game(..)
    , initGame
    , stepGame
    , gameScore

    -- * TODO move to a different module?
    , scorePhrases
    ) where

import           Batcave.Commands
import           Batcave.Hex
import           Batcave.Random
import           Batcave.Types

import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

------------------------------------------------------------------------
-- Types

-- | Overall game state.
data Game = Game {
      gameSource :: ![Unit]       -- ^ All coming units, relative coordinates
    , gameUnit   :: !(Maybe Unit) -- ^ The current unit in play
    , gameBoard  :: !Board        -- ^ Board state
    , gameScores :: ![UnitScore]  -- ^ For final scoring, leftmost==newest
    } deriving (Eq, Show)

data UnitScore = UnitScore {
      scoreSize  :: Int -- ^ Size of the scoring unit
    , scoreLines :: Int -- ^ Lines removed with this unit score
    } deriving (Eq, Show)

data GameError = InvalidProblem Text
  deriving (Eq, Show)


------------------------------------------------------------

-- | Try to initialise a game.
initGame :: Problem -> Seed -> Either GameError Game
initGame p@Problem{..} seed = do
    board  <- makeBoard
    source <- makeSource
    Right (Game source Nothing board [])
  where
    makeBoard = maybe (flail "Invalid board size") Right (initialBoard p)

    makeSource
      | validSeed = return (take problemSourceLength (unitSequence seed problemUnits))
      | otherwise = flail "Solution seed not in problem"

    validSeed = seed `V.elem` problemSourceSeeds

    flail = Left . InvalidProblem

unitSequence :: Seed -> V.Vector Unit -> [Unit]
unitSequence seed units = map (units V.!) rs
  where
    n  = V.length units
    rs = map ((`mod` n) . fromIntegral) (randoms s)
    s  = fromIntegral (unSeed seed)


------------------------------------------------------------

-- | Run a single step of the game.
stepGame :: Command -> Game -> Maybe Game
stepGame cmd game@Game{..}

    -- No current unit, grab one from the source.
    | Nothing        <- gameUnit
    , (fresh:source) <- gameSource
    , spawn          <- spawnUnit fresh gameBoard

    = stepGame cmd $ game { gameSource  = source
                          , gameUnit = Just spawn }


    -- Next board will be legal, so move
    | Just _    <- nextBoard
    , Just next <- nextUnit

    = Just $ game { gameUnit = Just next }


    -- Next board is not legal, but current board is ok, so lock
    | Nothing            <- nextBoard
    , Just (board, rows) <- clearBoard <$> currentBoard
    , Just score         <- makeScore rows <$> gameUnit

    = Just $ game { gameUnit = Nothing
                  , gameBoard   = board
                  , gameScores  = score : gameScores }


    -- Current board is not legal, game over
    | Nothing <- currentBoard

    = Nothing


    -- This shouldn't happen, dump the game state if it does.
    | otherwise

    = error ("stepGame: my brain just exploded:\n" ++ show game)

  where
    nextUnit = applyCommand cmd <$> gameUnit

    currentBoard = gameUnit >>= \u -> placeUnit u gameBoard
    nextBoard    = nextUnit >>= \u -> placeUnit u gameBoard

    makeScore rows unit = UnitScore {
        scoreSize  = V.length (unitMembers unit)
      , scoreLines = rows
      }


------------------------------------------------------------
-- Scoring

-- | Score for a game.
--
--   Can be called any time during running it if required.
--
--   **This does not include scores for power phrases.**
--
--   NOTE: gameScores are stored right-to-left.
--
--   Specification:
--
--     move_score = points + line_bonus
--       where
--         points      = size + 100 * (1 + ls) * ls / 2
--         line_bonus  = if ls_old > 1
--                       then floor ((ls_old - 1) * points / 10)
--                       else 0
--
gameScore :: Game -> GameScore
gameScore Game{..} = GameScore score
  where
    (MoveScore score _) = foldl' moveScore (MoveScore 0 0) (reverse gameScores)

data MoveScore = MoveScore !Int !Int

-- | Calculate the score for a single movement.
moveScore :: MoveScore -- ^ accumulator, last line count
          -> UnitScore -- ^ current scoring
          -> MoveScore -- ^ accumulator, lines counted
moveScore (MoveScore acc scoreLines_prev) UnitScore{..}
    = MoveScore (acc + points + bonus) scoreLines
  where
    points = scoreSize + 50  * (1 + ls) * ls
                    -- + 100 * (1 + ls) * ls / 2

    bonus | ls_old > 1 = (ls_old - 1) * points `div` 10
          | otherwise  = 0

    ls_old = scoreLines_prev
    ls     = scoreLines


------------------------------------------------------------------------
-- Power phrase scoring
--
-- TODO This should maybe be in a separate module?
--
-- Specification:
--
--   power_scorep = 2 * lenp * repsp + power_bonusp
--     where
--       power_bonusp = if repsp > 0 then 300 else 0
--

-- | Converts commands to canonical text and phrases to canoncical text,
--   then counts and maps the score function over the counts)
scorePhrases :: [Text]    -- ^ List of phrases (18 at most)
             -> [Command] -- ^ All commands
             -> Int       -- ^ Total power score
scorePhrases ps cs = sum (zipWith powerScore (map T.length ps) counts)
  where
    cs'    = commandsToText cs
    ps'    = map canonicalizeCommand ps
    counts = map (`T.count` cs') ps'
    -- This is inefficient (multiple traversals). Maybe OK?

-- | Power score for a given count/repetitions.
powerScore :: Int -- ^ Phrase length
           -> Int -- ^ Repetitions
           -> Int
powerScore len reps = 2 * len * reps + bonus
  where
    bonus = if reps > 0 then 300 else 0
