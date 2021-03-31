{-# LANGUAGE StrictData #-}
module Action where

import Protolude
import Miso
import Data.Set (Set(..))

import Crossword

data Action
  = NoAction
--  | Save
  | UpdateCell CellID CellState
  | UpdateComplement CellID CellState
  | UpdateDirection
  | UpdateHighlighted
  | UpdateNextSelected CellID
  | UpdateSelected CellID
  | UpdateSize Size
  | UpdateSymmetry Symmetry
  | UpdateWords
  | ArrowPress Arrows
  | KeyboardPress (Set Int)
  deriving (Eq, Show)

