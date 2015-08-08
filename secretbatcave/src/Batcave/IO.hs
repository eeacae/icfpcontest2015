module Batcave.IO where

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson

import Batcave.Types

loadProblemFile :: FilePath -> IO (Maybe Problem)
loadProblemFile fp = do
  d <- BL.readFile fp
  pure $ decode d
