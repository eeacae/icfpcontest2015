{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Array as Array
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Monoid ((<>))
import qualified Data.Vector as V
import           System.Environment (getArgs)

import qualified Batcave.Solver.Lucky as Lucky
import           Batcave.RunGame (Game(..), initGame, stepGame)
import           Batcave.Types

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

    frames = V.map encodeBoard
           . fst
           . V.foldl' runStep (V.singleton (board game0, current game0), Just game0)
           . V.fromList
           $ solutionCmds

    game0 = either (\msg -> error ("encodeFrames: " ++ show msg)) id (initGame problem solution)

    runStep (bs, game1) cmd =
      case stepGame cmd =<< game1 of
        Just game2 -> (bs `V.snoc` (board game2, current game2), Just game2)
        Nothing    -> (bs, Nothing)

encodeBoard :: (Board, Maybe Unit) -> A.Value
encodeBoard (board, current) =
    A.object [ "locked"  .= fromBoard board
             , "current" .= fromMaybe V.empty (fromCurrent <$> current) ]
  where
    fromBoard = V.fromList
              . mapMaybe full
              . Array.assocs
              . unBoard

    fromCurrent = V.map coord
                . unitMembers

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
