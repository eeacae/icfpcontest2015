{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Batcave.Solvers
    where

import           Batcave.Types
import           Batcave.Commands
import           Batcave.RunGame (Game(..), initGame)

import qualified Batcave.Solver.FloRida   as FloRida
import qualified Batcave.Solver.Nostrovia as Nostrovia

import qualified Data.Vector as V
import qualified Data.Text as T
import           Data.Monoid((<>))

import           Control.Parallel.Strategies

-- | apply a game solver function to all seeds given in a problem and
-- return all solutions
solveWith :: (Game -> [Command]) -- ^ solver interface
          -> T.Text              -- ^ tag to use (suffixed with seed)
          ->  Problem -> [Solution]
solveWith solveGame name problem@Problem{..}
    = zipWith mkSolutions seeds (parMap rdeepseq solveGame games)
    -- using parmap here for more fine-grained parallelism
    where games = map (either die id . initGame problem) seeds

          seeds = V.toList problemSourceSeeds

          mkSolutions :: Seed -> [Command] -> Solution
          mkSolutions seed cmds 
              = Solution{
                  solutionProb = problemId
                , solutionSeed = seed
                , solutionTag  
                    = Just (tag <> T.pack ('-':show (unSeed seed)))
                , solutionCmds = cmds
                }

          tag = name <> T.pack ('-':show problemId)

          die x = error (unlines ["Solvers", T.unpack tag, show x])

useFloRida :: Problem -> [Solution]
useFloRida = solveWith FloRida.solveGame "flo-rida"

useNostrovia :: Problem -> [Solution]
useNostrovia = solveWith Nostrovia.solveGame "nostrovia"

