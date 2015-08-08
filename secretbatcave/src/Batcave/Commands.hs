{-# LANGUAGE FlexibleInstances #-}

-- | Data types and instances representing commands
module Batcave.Commands
(
    CompassDirection(..),
    RotationDirection(..),
    Command(..),
    canonicalizeCommand,
    textToCommands',
    commandsToText
) where

import           Data.Aeson
import           Data.Text  (Text)
import qualified Data.Text  as T
import qualified Data.Map as Map
import           Data.Maybe

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

textToCommands :: Text -> [Command]
textToCommands = T.foldr step []
  where
    step x xs = charToCommand x : xs

{- | deals with the characters not used in commandToChar
{p, ', !, ., 0, 3}	move W
{b, c, e, f, y, 2}	move E
{a, g, h, i, j, 4}	move SW
{l, m, n, o, space, 5}    	move SE
{d, q, r, v, z, 1}	rotate clockwise
{k, s, t, u, w, x}	rotate counter-clockwise
\t, \n, \r	(ignored)
-}
table :: Map.Map Char Char
table = Map.fromList (zip "abcdefghijklmnopqrstuvwxyz !.012345\'" 
                          "abbdbbaaaakllllpddkkkdkkbdlpppdbpalp")

-- | deals with the characters not used in commandToChar
textToCommands' = textToCommands   -- use canonical char.s
                  . T.map replace  -- replacement as defined above
                  . T.filter (not . (`elem` "\t\n\r")) -- remove ignored
    where replace c = fromMaybe (error "unknown command") (Map.lookup c table)

-- | replaces characters other than the ones we use in commandsToText
-- by the used ones (by converting back and forth)
canonicalizeCommand :: T.Text -> T.Text
canonicalizeCommand = commandsToText . textToCommands'


-- | Maps a command to just one of the possible single character
-- representations
commandToChar :: Command -> Char
commandToChar (Move W)                  = 'p'
commandToChar (Move E)                  = 'b'
commandToChar (Move SW)                 = 'a'
commandToChar (Move SE)                 = 'l'
commandToChar (Rotate Clockwise)        = 'd'
commandToChar (Rotate CounterClockwise) = 'k'

-- | Inverse of charToCommand, partial; we assume that we are operating under
-- the image of commandToChar.
charToCommand :: Char -> Command
charToCommand 'p' = Move W
charToCommand 'b' = Move E
charToCommand 'a' = Move SW
charToCommand 'l' = Move SE
charToCommand 'd' = Rotate Clockwise
charToCommand 'k' = Rotate CounterClockwise
charToCommand x   = error $ "unknown character while decoding Command: " ++ [x]

instance ToJSON [Command] where
    toJSON = String . commandsToText

instance FromJSON [Command] where
    parseJSON (String txt) = return $ textToCommands txt
    parseJSON _            = fail "Expected String when decoding Commands"

