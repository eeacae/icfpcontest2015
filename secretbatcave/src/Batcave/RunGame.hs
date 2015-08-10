{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wall #-}

module Batcave.RunGame
    ( Game(..)
    , ActiveUnit(..)
    , GameEnd(..)
    , GameScore(..)
    , gameScore

    -- * Game running
    , initGame
    , runGame
    , stepGame
    , ensureUnit

    -- * TODO move to a different module?
    , scorePhrases
    ) where

import           Control.Monad (join)
import           Data.List
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import           Batcave.Commands
import           Batcave.Hex
import           Batcave.Random
import           Batcave.Types

------------------------------------------------------------------------
-- Types

-- | Overall game state.
data Game = Game {
      gameSource :: ![Unit]             -- ^ All coming units, relative coordinates
    , gameActive :: !(Maybe ActiveUnit) -- ^ The unit under active player control
    , gameBoard  :: !Board              -- ^ Board state
    , gameScores :: ![UnitScore]        -- ^ For final scoring, leftmost==newest
    } deriving (Eq, Show)

data ActiveUnit = ActiveUnit {
      activeUnit    :: !Unit       -- ^ The current active unit
    , activeHistory :: !(Set Unit) -- ^ The history of states we have been in previously
    } deriving (Eq, Show)

data UnitScore = UnitScore {
      scoreSize  :: !Int -- ^ Size of the scoring unit
    , scoreLines :: !Int -- ^ Lines removed with this unit score
    } deriving (Eq, Show)

newtype GameScore = GameScore {
      unGameScore :: Int
    } deriving (Eq, Show, Num)

data GameEnd =
      YouWin        -- ^ Successfully exhausted the source.
    | BoardOverflow -- ^ We ran out of space on the board.
    | IllegalMove   -- ^ Player tried to make an illegal move.
    | Fatal Text    -- ^ A fatal error occurred.
  deriving (Eq, Show)


------------------------------------------------------------
-- Game initialisation

-- | Try to initialise a game.
initGame :: Problem -> Seed -> Either GameEnd Game
initGame p@Problem{..} seed = do
    board  <- makeBoard
    source <- makeSource
    Right (Game source Nothing board [])
  where
    makeBoard = maybe (flail "Invalid board size") Right (initialBoard p)

    makeSource
      | validSeed = return (take problemSourceLength (unitSequence seed problemUnits))
      | otherwise = flail "Seed not in problem"

    validSeed = seed `V.elem` problemSourceSeeds

    flail = Left . Fatal

unitSequence :: Seed -> V.Vector Unit -> [Unit]
unitSequence seed units = map (units V.!) rs
  where
    n  = V.length units
    rs = map ((`mod` n) . fromIntegral) (randoms s)
    s  = fromIntegral (unSeed seed)


------------------------------------------------------------
-- Active unit / history

makeActive :: Unit -> ActiveUnit
makeActive unit = ActiveUnit unit Set.empty

moveNext :: Unit -> ActiveUnit -> Maybe ActiveUnit
moveNext unit (ActiveUnit _ history)
  | Set.member unit history = Nothing
  | otherwise               = Just (ActiveUnit unit (Set.insert unit history))


------------------------------------------------------------
-- Stepping through the game states

-- | Run a number of steps of the game.
runGame :: [Command] -> Game -> Either GameEnd Game
runGame cmds game = foldl' loop (Right game) cmds
  where
    loop :: Either GameEnd Game -> Command -> Either GameEnd Game
    loop (Left  e) _   = Left e
    loop (Right g) cmd = stepGame cmd g

-- | Run a single step of the game.
stepGame :: Command -> Game -> Either GameEnd Game
stepGame cmd game0 = step =<< ensureUnit game0
  where
    step game@Game{..}
        -- Next board will be legal, so move
        | Just _    <- nextBoard
        , Just next <- nextActive

        = Right $ game { gameActive = Just next }


        -- Next board will be legal, but will result in a duplicate state, game over
        | Just _  <- nextBoard
        , Nothing <- nextActive

        = Left IllegalMove


        -- Next board is not legal, but current board is ok, so lock
        | Nothing            <- nextBoard
        , Just (board, rows) <- clearBoard <$> currentBoard
        , Just score         <- makeScore rows <$> gameUnit

        = Right $ game { gameActive = Nothing
                       , gameBoard  = board
                       , gameScores = score : gameScores }


        -- Current board is not legal, game over
        | Nothing <- currentBoard

        = Left BoardOverflow


        -- This shouldn't happen, dump the game state if it does.
        | otherwise

        = Left (Fatal ("my brain just exploded:\n" <> T.pack (show game)))

      where
        gameUnit = activeUnit <$> gameActive
        nextUnit = applyCommand cmd <$> gameUnit

        currentBoard = gameUnit >>= \u -> placeUnit (u,u) gameBoard
        nextBoard    = nextUnit >>= \u -> placeUnit u gameBoard

        nextActive = join (moveNext <$> (fst <$> nextUnit) <*> gameActive)

        makeScore rows unit = UnitScore {
            scoreSize  = Set.size (unitMembers unit)
          , scoreLines = rows
          }


-- | Ensure that the game has a current unit.
ensureUnit :: Game -> Either GameEnd Game
ensureUnit game@Game{..}

    -- NOTE The order here is important, conditions lower down
    --      are assuming conditions higher up have failed.

    -- Already have a unit, nothing to do!
    | Just _ <- gameActive

    = Right game


    -- No units left in the source, game over!
    | [] <- gameSource

    = Left YouWin


    -- No current unit, grab one from the source.
    | (fresh:source) <- gameSource
    , Just spawn     <- spawnUnit fresh gameBoard

    = Right (game { gameSource = source
                  , gameActive = Just (makeActive spawn) })


    -- Cannot spawn unit, board must have overflowed :(
    | otherwise

    = Left BoardOverflow


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
