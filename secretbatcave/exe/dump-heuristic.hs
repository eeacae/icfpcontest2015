{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import           Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Array as Array
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.Vector as V
import           System.Environment (getArgs)

import           Batcave.RunGame (Game(..), ActiveUnit(..), GameEnd(..))
import           Batcave.RunGame (initGame, stepGame, gameScore, unGameScore)
import qualified Batcave.Solver.Nostrovia as Nostrovia
import           Batcave.Types

import           Debug.Trace

------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
      []       -> putStrLn "Usage: dump-heuristic PROBLEM"
      (path:_) -> do
        problem <- readProblem path
        L.putStrLn (A.encode (encodeFrames Nostrovia.solve problem))

------------------------------------------------------------------------

encodeFrames :: (Problem -> Solution) -> Problem -> A.Value
encodeFrames solve problem@Problem{..} =
    A.object [ "width"  .= problemWidth
             , "height" .= problemHeight
             , "frames" .= frames ]
  where
    solution@Solution{..} = solve problem

    frames = V.map encodeGame
           $ V.unfoldr runStep (game0, commands)

    commands = V.fromList solutionCmds

    game0 = either (\msg -> error ("encodeFrames: " ++ show msg)) id (initGame problem solutionSeed)

    runStep (game1, cmds)
        | V.null cmds = Nothing
        | otherwise   = case stepGame (V.head cmds) game1 of
                          Left end -> traceShow end Nothing
                          Right  g -> Just (g, (g, V.tail cmds))

encodeGame :: Game -> A.Value
encodeGame game@Game{..} =
    A.object [ "locked"  .= fromBoard gameBoard
             , "current" .= fromMaybe V.empty (fromActive <$> gameActive)
             , "score"   .= takeScore game ]
  where
    fromBoard = V.fromList
              . mapMaybe full
              . Array.assocs
              . unBoard

    fromActive = V.map coord
               . V.fromList
               . Set.toList
               . unitMembers
               . activeUnit

    takeScore = unGameScore
              . gameScore

    full (cell, Full) = Just (coord cell)
    full _            = Nothing

    coord (Cell x y) = V.fromList [x, y]

------------------------------------------------------------------------

readProblem :: FilePath -> IO Problem
readProblem path = do
    bs <- L.readFile path
    return (either die id (A.eitherDecode bs))
  where
    die msg = error ("readProblem: failed reading json: " ++ msg)

