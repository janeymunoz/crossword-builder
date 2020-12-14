{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Update where

import Protolude
import Miso hiding (at)
import qualified Control.Lens as Lens
import Control.Lens ((.=), (^.), (%=), (.~), (?~), (&))
import Control.Lens.Getter (use)
import Control.Lens.At (at)
import Control.Lens.Prism (_Just)
import Data.Char (chr, isAlpha, toUpper)
import Data.Set as Set

import Action
import Crossword
import Model

updateModel :: Action -> Transition Action Model ()
updateModel action =
  case action of
    NoAction -> pure ()
    UpdateCell cellID state' ->
      board . cells . at cellID . _Just .= state'
    UpdateDirection -> do
      board . direction %= otherDir 
      si <- use $ board . Crossword.size
      cs <- use $ board . cells
      dir <- use $ board . direction
      sel <- use $ board . selected
      board . highlighted .= newHighlighted si cs dir sel
    UpdateSelected cIDSelected' newHighlighted' -> do
      board . selected .= cIDSelected'
      board . highlighted .= newHighlighted'
    ArrowPress (Arrows arrX arrY) -> do
      si <- use $ board . Crossword.size
      cs <- use $ board . cells
      dir <- use $ board . direction
      (CellID rSel cSel) <- use $ board . selected
      let newSelected = (CellID (rSel - arrY) (cSel + arrX))
      board . selected .= ifEndBegin si newSelected
      board . highlighted .= newHighlighted si cs dir newSelected
    KeyboardPress keys -> case uncons $ Set.toList keys of
      Just (h, []) -> if isAlpha $ chr h
        then do
          sel <- use $ board . selected
          board . cells . at sel . _Just .= (Alpha . toUpper $ chr h)
        else pure ()
      _ -> pure ()

