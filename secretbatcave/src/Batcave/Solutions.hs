-- | Represent solutions for output.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Batcave.Solutions where

import           Control.Parallel.Strategies
import           Data.Aeson
import           Data.Monoid
import           Data.Text                   (Text)
import           Data.Word

import           Batcave.Commands

-- | A solution to a particular problem case.
data Solution = Solution
    { problemId    :: Int
    , problemSeed  :: Word32
    , solutionTag  :: Maybe String
    , solutionCmds :: [Command]
    }
  deriving (Show, Eq)

instance ToJSON Solution where
    toJSON Solution{..} = object $
                      [ "problemId" .= problemId
                      , "seed" .= problemSeed
                      , "solution" .= solutionCmds
                      ] <> maybe [] (\v -> ["tag" .= v]) solutionTag
