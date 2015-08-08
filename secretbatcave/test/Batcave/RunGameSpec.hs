module Batcave.RunGameSpec where

import Control.Monad.State
import Control.Monad.Except
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property
import Batcave.Hex
import Batcave.RunGame
import Batcave.Types

spec :: Spec
spec = describe "Tests for Batcave.RunGame (command interpreting logic)" $ do
  it "placeholder" $ property $ prop_moveUntilLocked_returns_placeable

prop_moveUntilLocked_returns_placeable = do
  (b, unit) <- arbitrary `suchThat` (uncurry unitPlaceable)
  commands <- arbitrary
  let game = Game {
    source = [],
    current = Nothing,
    board = b,
    cmds = commands,
    unitScores = []
  }
  let result = flip evalState game (runExceptT $ moveUntilLocked unit)
  case result of
    Left x -> return $ counterexample (show (x,b,unit,commands)) (x == OutOfCommands)
    Right x -> return $ counterexample (show (b,unit,commands)) $ liftBool $ unitPlaceable b x
