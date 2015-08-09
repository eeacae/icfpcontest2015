{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Batcave.BFS
import Batcave.Types
import qualified Data.Vector as V
import Data.Set (Set)
import Data.Maybe
import qualified Data.Set as Set


import           Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Array as Array
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Monoid ((<>))
import qualified Data.Vector as V
import           System.Environment (getArgs)

import           Batcave.RunGame (Game(..), initGame, stepGame, gameScore)
import qualified Batcave.Solver.Lucky as Lucky
import           Batcave.Types


b :: Board
b = emptyBoard (Bounds (Cell 0 0) (Cell 20 20))
u = singleton $ Unit (V.fromList [Cell 1 0]) (Cell 0 0)

searched :: [Unit]
searched = fst $ bfs u mempty b

------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
      []       -> putStrLn "Usage: dump PROBLEM"
      (path:_) -> do
        problem <- readProblem path
        L.putStrLn (A.encode (encodeFrames Lucky.solve problem))

------------------------------------------------------------------------

encodeFrames :: (Problem -> Solution) -> Problem -> A.Value
encodeFrames solve problem@Problem{..} =
    A.object [ "width"  .= problemWidth
             , "height" .= problemHeight
             , "frames" .= frames ]
  where
    solution@Solution{..} = solve problem

    frames = V.fromList $ map encodeGame searched

    commands = V.fromList solutionCmds

    seed = problemSourceSeeds V.! 0

    game0 = either (\msg -> error ("encodeFrames: " ++ show msg)) id (initGame problem seed)

    board0 = gameBoard game0

    step1 = fromJust $ stepGame (V.head commands) game0

    initial_unit :: Unit
    initial_unit = fromJust (gameUnit step1)

    searched :: [Unit]
    searched = fst $ bfs (singleton initial_unit) mempty board0

    runStep (game1, cmds)
        | V.null cmds = Nothing
        | otherwise   = (\g -> (g, (g, V.tail cmds))) <$> stepGame (V.head cmds) game1

encodeGame :: Unit -> A.Value
encodeGame us =
    A.object [ "locked"  .= (mapMaybe coord $ thing us)
             , "current" .= ([] :: [Int])
             , "score"   .= (42 :: Double) ]
  where
    fromBoard = V.fromList
              . mapMaybe full
              . Array.assocs
              . unBoard

    fromCurrent = V.map coord
                . unitMembers

    takeScore = unGameScore
              . gameScore

    full (cell, Full) = Just (coord cell)
    full _            = Nothing

    coord (Cell x y) | x < 0 || y < 0 = Nothing
                    | otherwise       = Just $ V.fromList [x, y]

    thing :: Unit -> [Cell]
    thing xs = do
        V.toList $ unitMembers xs

------------------------------------------------------------------------

readProblem :: FilePath -> IO Problem
readProblem path = do
    bs <- L.readFile path
    return (either die id (A.eitherDecode bs))
  where
    die msg = error ("readProblem: failed reading json: " ++ msg)

