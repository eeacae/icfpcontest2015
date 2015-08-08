{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Batcave.Draw where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.Array

import Batcave.Types
import qualified Data.Vector as V

translateCell (Cell x y) = translate (V2 x' y')
    where
        x'     = width * fromIntegral x + offset
        y'     = negate $ fromIntegral y * 3 / 4 * height
        offset = if even y then 0 else width / 2
        width  = sqrt 3 / 2 * height
        height = 2

drawCell' :: Cell -> Colour Double -> Diagram B
drawCell' c@(Cell x y) colour = translateCell c $ label <> cell
    where
        cell   = rotateBy (1/12) $ hexagon 1 # fc colour
        label  = scale 0.5 $ text $ show x ++ "," ++ show y

drawCell :: Cell -> CellStatus -> Diagram B
drawCell c Empty = drawCell' c white
drawCell c Full = drawCell' c yellow

drawUnit :: Unit -> Diagram B
drawUnit Unit{..} = translateCell unitPivot (circle 0.7) # lc red
                    <> (V.foldl1' (<>) (V.map (flip drawCell' green) unitMembers))
drawBoard :: Board -> Diagram B
drawBoard (Board b) = mconcat $ map (uncurry drawCell) $ assocs b

-- You can render units ontop of boards and they should match up.
-- renderSVG "test.svg" (mkWidth 400) $ drawUnit unit <> drawBoard board

exampleBoard = array (Cell 0 0, Cell 3 4)
                [ (Cell 0 0, Empty)
                , (Cell 0 1, Empty)
                , (Cell 0 2, Full)
                , (Cell 0 3, Full)
                , (Cell 0 4, Full)
                , (Cell 1 0, Empty)
                , (Cell 1 1, Empty)
                , (Cell 1 2, Empty)
                , (Cell 1 3, Full)
                , (Cell 1 4, Full)
                , (Cell 2 0, Empty)
                , (Cell 2 1, Empty)
                , (Cell 2 2, Empty)
                , (Cell 2 3, Full)
                , (Cell 2 4, Empty)
                , (Cell 3 0, Empty)
                , (Cell 3 1, Empty)
                , (Cell 3 2, Full)
                , (Cell 3 3, Empty)
                , (Cell 3 4, Full)
                ]

renderBoard :: FilePath -> Board -> IO ()
renderBoard f b = renderSVG f (mkWidth 400) $ drawBoard b
