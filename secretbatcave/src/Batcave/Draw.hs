{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Batcave.Draw where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.Array

import Batcave.Types

drawCell :: Cell -> CellStatus -> Diagram B
drawCell (Cell x y) s = translate (V2 x' y') $ label <> cell
    where
        cell   = rotateBy (1/12) $ hexagon 1 # fc colour
        x'     = width * fromIntegral x + offset
        y'     = negate $ fromIntegral y * 3 / 4 * height
        offset = if even y then 0 else width / 2
        width  = sqrt 3 / 2 * height
        height = 2
        label  = scale 0.5 $ text $ show x ++ "," ++ show y
        colour = case s of
                    Full  -> yellow
                    Empty -> white

drawBoard :: Board -> Diagram B
drawBoard b = mconcat $ map (uncurry drawCell) $ assocs b

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
