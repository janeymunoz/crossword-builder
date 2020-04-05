module Action where

import Protolude

import qualified Crossword

data Action
  = NoAction
  | UpdateCell Crossword.Cell Crossword.CellState
  | UpdateDirection
  | UpdateSelected Crossword.CellID
  deriving (Eq, Show)

