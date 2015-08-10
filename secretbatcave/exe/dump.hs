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
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           System.Environment (getArgs)
import           System.IO (stderr)

import           Batcave.Commands (textToCommands')
import           Batcave.RunGame (Game(..), ActiveUnit(..), GameEnd(..))
import           Batcave.RunGame (initGame, stepGame, ensureUnit, gameScore, unGameScore)
import qualified Batcave.Solver.FloRida as FloRida
import qualified Batcave.Solver.Lucky as Lucky
import qualified Batcave.Solver.Nostrovia as Nostrovia
import           Batcave.Types

import           Debug.Trace

------------------------------------------------------------------------

powerPhrases :: [Text]
powerPhrases = ["ei!", "yuggoth"]

------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
      (solver:path:[]) -> do
        problem <- readProblem path
        let solution  = solveWith solver problem

        L.putStrLn (A.encode (encodeFrames problem (roundtrip solution)))
        L.hPutStrLn stderr (A.encode (V.singleton solution))

      _ -> putStrLn "Usage: dump SOLVER PROBLEM"
  where
    roundtrip solution = case A.eitherDecode (A.encode solution) of
        Left  msg -> error ("failed to roundtrip solution: " ++ msg)
        Right sln -> sln

    solveWith solver = case lookup solver solvers of
        Nothing -> error ("Unknown solver: " ++ show solver ++ ", valid solvers are: " ++ show (map fst solvers))
        Just s  -> s

    solvers = [ ("lucky",     Lucky.solve)
              , ("florida",   FloRida.solve powerPhrases)
              , ("nostrovia", Nostrovia.solve powerPhrases)
              , ("example6",  const problem6_seed0_example) ]

------------------------------------------------------------------------

encodeFrames :: Problem -> Solution -> A.Value
encodeFrames problem@Problem{..} solution@Solution{..} =
    A.object [ "width"  .= problemWidth
             , "height" .= problemHeight
             , "frames" .= frames ]
  where
    frames = V.map encodeGame
           $ V.unfoldr runStep (game0, commands)

    commands = V.fromList solutionCmds

    game0 = either (\msg -> error ("encodeFrames: " ++ show msg)) id (initGame powerPhrases problem solutionSeed)

    runStep (game1@Game{..}, cmds0)
        | V.null cmds0 = Nothing
        | otherwise    =
            case gameActive of
              Nothing -> continue cmds0          (ensureUnit game1)
              Just _  -> continue (V.tail cmds0) (stepGame (V.head cmds0) game1)
      where
        continue _     (Left end) = traceShow end Nothing
        continue cmds1 (Right  g) = Just (g, (g, cmds1))

encodeGame :: Game -> A.Value
encodeGame game@Game{..} =
    A.object [ "locked"  .= fromBoard gameBoard
             , "members" .= fromMaybe V.empty (activeMembers <$> gameActive)
             , "pivot"   .= (activePivot <$> gameActive)
             , "score"   .= takeScore game ]
  where
    fromBoard = V.fromList
              . mapMaybe full
              . Array.assocs
              . unBoard

    activeMembers = V.map coord
                  . V.fromList
                  . Set.toList
                  . unitMembers
                  . activeUnit

    activePivot = coord
                . unitPivot
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

problem6_seed0_example :: Solution
problem6_seed0_example = Solution {
      solutionProb = 6
    , solutionSeed = Seed 0
    , solutionTag  = Just "Galois Example"
    , solutionCmds = textToCommands' commands
    }
  where
    commands :: Text
    commands = T.concat [
        "iiiiiiiimmiiiiiimimmiiiimimimmimimimimmimimimeemimeeeemimim"
      , "imimiiiiiimmeemimimimimiimimimmeemimimimmeeeemimimimmiiiiii"
      , "pmiimimimeeemmimimmemimimimiiiiiimeeemimimimimeeemimimimmii"
      , "iimemimimmiiiipimeeemimimmiiiippmeeeeemimimimiiiimmimimeemi"
      , "mimeeeemimimiiiipmeeemmimmiimimmmimimeemimimimmeeemimiiiiip"
      , "miiiimmeeemimimiiiipmmiipmmimmiippimemimeeeemimmiipppmeeeee"
      , "mimimmiimipmeeeemimimiimmeeeeemimmeemimmeeeemimiiippmiippmi"
      , "iimmiimimmmmmeeeemimmiippimmimimeemimimimmeemimimimmeemimim"
      , "imiimimimeeemmimimmmiiiiipimeemimimimmiiiimimmiiiiiiiimiimi"
      , "mimimeeemmimimimmiiiiiimimmemimimimimmimimimeemimiiiiiiiimi"
      , "iiimimimiimimimmimmimimimimmeeeemimimimimmmimimimimeemimimi"
      , "mimmmemimimmiiiiiiimiimimimmiiiiiimeeeeemimimimimmimimimmmm"
      , "emimimmeeeemimimimmiimimimmiiiiiipmeeeeemimimimimmiiiiimmem"
      , "imimimimmmmimimmeeeemimimimimeeemimimimmiimimimeeemmimimmii"
      , "iiiiimimiiiiiimimmiiiiiiiimmimimimimiiiimimimeemimimimimmee"
      , "emimimimimiiiiiiimiiiimimmemimimimmeemimimimeeemmimimmiiiii"
      , "immiiiipmmiiimmmimimeemimimeeemmimmiiiippmiiiimiiippimiimim"
      , "eemimimeeeemimimiiiipmeemimimiimiimimmimeeemimimmippipmmiim"
      , "emimmipimeeeemimmeemimiippimeeeeemimimmmimmmeeeemimimiiipim"
      , "miipmemimmeeeemimimiipipimmipppimeeemimmpppmmpmeeeeemimmemm" ]
