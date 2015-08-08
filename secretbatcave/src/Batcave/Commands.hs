{-# LANGUAGE FlexibleInstances #-}

-- | Data types and instances representing commands
module Batcave.Commands
(
    CompassDirection(..),
    RotationDirection(..),
    Command(..)
) where

import           Data.Aeson
import           Data.Text  (Text)
import qualified Data.Text  as T

data CompassDirection
    = E | W | SE | SW
  deriving (Eq, Show)

data RotationDirection
    = Clockwise | CounterClockwise
  deriving (Eq, Show)

data Command
    = Move CompassDirection
    | Rotate RotationDirection
  deriving (Eq, Show)

-- | O(n), turn a list of commands into a String, e.g.
--
--    > toJSON [ Move W, Move E, Move SW, Move SE,
--    >         Rotate Clockwise, Rotate CounterClockwise]
--    String "pbaldk"
commandsToText :: [Command] -> Text
commandsToText cmds = T.unfoldrN (length cmds) step cmds
  where
    step (x:xs) = Just (commandToChar x, xs)
    step [] = Nothing

-- | Maps a command to just one of the possible single character
-- representations
commandToChar :: Command -> Char
commandToChar (Move W)                  = 'p'
commandToChar (Move E)                  = 'b'
commandToChar (Move SW)                 = 'a'
commandToChar (Move SE)                 = 'l'
commandToChar (Rotate Clockwise)        = 'd'
commandToChar (Rotate CounterClockwise) = 'k'

instance ToJSON [Command] where
  toJSON = String . commandsToText

