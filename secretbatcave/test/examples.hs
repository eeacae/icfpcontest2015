module Main where

import Control.Applicative
import Data.Monoid
import System.Directory
import Test.Hspec

import Batcave.IO

suite :: Spec
suite = do
  describe "loadProblemFile" $ do
    it "decodes example inputs" $ do
      fs <- ((filter ((==) ".json" . reverse . take 5 . reverse))) <$> getDirectoryContents "../problems"
      rs <- mapM loadProblemFile $ ("../problems/" <>) <$> fs
      length rs `shouldBe` length fs

main :: IO ()
main = hspec suite

