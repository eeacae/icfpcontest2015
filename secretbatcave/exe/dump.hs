{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Monoid ((<>))
import qualified Data.Vector as V
import           System.Environment (getArgs)

import           Batcave.Types

------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
      []    -> putStrLn "Usage: dump [PROBLEM]"
      paths -> do
        ps <- mapM readProblem paths
        let xs = map encodeFrame ps
        L.putStrLn (A.encode xs)
  where
    encodeFrame Problem{..} = A.object [ "width"  .= problemWidth
                                       , "height" .= problemHeight
                                       , "filled" .= V.map coord problemFilled ]
    coord (Cell x y) = [x, y]

------------------------------------------------------------------------

readProblem :: FilePath -> IO Problem
readProblem path = do
    bs <- L.readFile path
    return (either die id (A.eitherDecode bs))
  where
    die msg = error ("readProblem: failed reading json: " ++ msg)
