{-# LANGUAGE StrictData #-}
module Action where

import Protolude
import Miso
import Data.Set (Set(..))

import Crossword

data Action
  = NoAction
  | UpdateCell CellID CellState
  | UpdateDirection
  | UpdateSelected CellID (Maybe (CellID, CellID))
  | ArrowPress Arrows
  | KeyboardPress (Set Int)
  deriving (Eq, Show)

