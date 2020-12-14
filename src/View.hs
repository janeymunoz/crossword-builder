{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module View where

import Protolude
import Control.Lens ((^.))
import Control.Lens.Wrapped (op)
import Data.Char (chr, isAlpha, toUpper)
import Data.Map.Strict as Map hiding (map)
import Data.IntMap.Strict as IntMap hiding (map)
import Miso
import Miso.String (ms)

import Action
import Crossword
import Model

cellSize :: Int
cellSize = 30

gridSize :: Int
gridSize = 15 * cellSize

viewModel :: Model -> View Action
viewModel model =
  div_
    [ class_ "cells"
    , style_ $
        Map.fromList
          [ ( "display", "grid" )
          , ( "grid-template-columns", ms $ intercalate " " (replicate 15 "auto") )
          , ( "height", ms $ ((show gridSize) <> "px" :: Text))
          , ( "width", ms $ ((show gridSize) <> "px" :: Text))
          ]
    , onDoubleClick UpdateDirection
    ] $
    map (cell si cs dir hi sel) $ Map.toList cs
  where
    b = model^.board
    si = b^.Crossword.size
    cs = b^.cells
    dir = b^.direction
    hi = b^.highlighted
    sel = b^.selected

cell :: Size -> Cells -> Direction -> Maybe (CellID, CellID) -> CellID -> (CellID, CellState) -> View Action
cell si cs dir highlighted selected (currentCellID, currentCellState) =
  div_
    [ class_ "cell"
    , style_ $
        Map.fromList
          [ ("background-color", background)
          , ("color", "black")
          , ("text-align", "center")
          , ("font-family", "Courier New, Courier, monospace")
          , ("border", "1px solid black")
          , ("box-sizing" , "border-box")
          , ("line-height", ms (show cellSize <> "px" :: Text))
          , ("border-collapse", "collapse")
          ]
    , onClick $ UpdateSelected currentCellID (newHighlighted si cs dir currentCellID)
    ]
    valueCell
  where
    cellSize' = ms (show cellSize :: Text)
    valueCell = case currentCellState of
      Alpha c -> [ text $ ms [c] ]
      _ -> []
    background
      | currentCellID == selected = selected'
      | fromMaybe False . fmap (elem currentCellID) $ range <$> highlighted = highlighted'
      | otherwise = case currentCellState of
          Empty -> empty'
          Off -> black
          _ -> white

selected' = "rgba(60, 210, 195, 1)"
white = "rgba(255, 255, 255, 1)"
black = "rgba(0, 0, 0, 1)"
empty' = "rgba(0, 0, 0, 0.1)"
highlighted' = "rgba(60, 210, 195, 0.4)"

