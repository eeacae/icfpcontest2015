{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Batcave.Solver.Lucky (solve) where

import           Batcave.Types
import           Batcave.Commands

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Text   as T

-- | a somewhat-random solution that drops pieces downwards and hopes to
-- fill a line by chance at some point. Only considers the max width
-- of units and the board dimensions
solve :: Problem -> Solution
solve p@Problem{..} =
    Solution { solutionProb = problemId
             , solutionSeed = seed 
             , solutionTag = Just $ T.pack ("lucky-" ++ show (problemId, seed))
             , solutionCmds = textToCommands' $ T.pack $
                              concat $ take problemSourceLength $
                              drop problemWidth
             }
        where seed = V.head problemSourceSeeds
              maxwidth = V.maximum $ V.map unitWidth problemUnits
              -- generate commands which try to drop units unit down a
              -- given height, using given width, and repeating with
              -- reduced bWidth until below 0
              drop :: Int  -> [String]
              drop width
                  | width < 0 = drop problemWidth
                  | width < maxwidth -- drop one, straight down
                      = concat (replicate (problemHeight `div` 2) "ei! ")
                        : drop problemWidth -- restart from full width
                  | otherwise -- drop two pieces, one left, one right
                      = downWest : downEast : drop (width - 2*maxwidth)
                  where sw   = cycle "aghij"
                        se   = cycle "moln "
                        downWest = straight ++ take (maxwidth `div` 2) sw
                        downEast = straight ++ take (maxwidth `div` 2) se
                        straight 
                            = concat $
                              replicate ((problemHeight - maxwidth) `div`2) $
                              "ei! "
                       
unitWidth :: Unit -> Int
unitWidth unit = let xs = U.map cellX (unitMembers unit)
                 in U.maximum xs - U.minimum xs

cellX :: Cell -> Int
cellX (Cell x _) = x
cellY :: Cell -> Int
cellY (Cell _ y) = y
