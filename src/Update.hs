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
import Data.Char (chr, isAlpha, toUpper, isPunctuation)
import Data.Set as Set

import Action
import Crossword
import Model

updateModel :: Action -> Transition Action Model ()
updateModel action =
  case action of
    NoAction -> pure ()
--    Save -> do
--      b <- use $ board
--      scheduleIO $ (writeFile "/home/janey/gits/github.com/janeymunoz/crossword-builder/out.cw" $ show b) >> pure NoAction
    UpdateCell cellID state' -> do
      board . cells . at cellID . _Just .= state'
      updateModel $ UpdateComplement cellID state'
      updateModel UpdateWords
    UpdateComplement cIDSelected' state' -> do
      si <- use $ board . Crossword.size
      sym <- use $ board . symmetry
      case complementCellID si sym cIDSelected' of
        Nothing -> pure ()
        Just complement -> board . cells . at complement . _Just .= complementState
      where
        complementState =
          case state' of
            Alpha _ -> On
            other -> other
    UpdateDirection -> do
      board . direction %= otherDir 
      updateModel UpdateHighlighted
    UpdateHighlighted -> do
      si <- use $ board . Crossword.size
      cs <- use $ board . cells
      dir <- use $ board . direction
      sel <- use $ board . selected
      board . highlighted .= newHighlighted si cs dir sel
    UpdateNextSelected cIdSelected' -> do
      sel <- use $ board . selected
      si <- use $ board . Crossword.size
      dir <- use $ board . direction
      cs <- use $ board . cells
      updateModel $ UpdateSelected $ nextSelected si dir sel cs 
    UpdateSelected cIDSelected' -> do
      board . selected .= cIDSelected'
      updateModel UpdateHighlighted
    UpdateSize size' -> do
      board . cells .= (startCells size')
      board . Crossword.size .= size'
      updateModel UpdateHighlighted
      board . wordBank .= []
      board . answerIDs .= mempty
    UpdateSymmetry symmetry' ->
      board . symmetry .= symmetry'
    UpdateWords -> do
      si <- use $ board . Crossword.size
      cs <- use $ board . cells
      board . wordBank .= (makeWords si cs)
    ArrowPress (Arrows arrX arrY) -> do
      gridSelected' <- use gridSelected
      if gridSelected'
         then do
           si <- use $ board . Crossword.size
           (CellID rSel cSel) <- use $ board . selected
           let newSelected = (CellID (rSel - arrY) (cSel + arrX))
           board . selected .= ifEndBegin si newSelected
           updateModel UpdateHighlighted
        else
          pure ()
    KeyboardPress keys ->
      case uncons $ Set.toList keys of
        Just (h, []) -> do
          sel <- use $ board . selected
          gridSelected' <- use gridSelected
          case h of
            192 -> do -- '`'
              gridSelected .= (not gridSelected')
            h' ->
              if gridSelected'
                 then case h' of
                   191 -> updateModel UpdateDirection -- '/'
                   188 -> do
                     updateModel $ UpdateCell sel On -- ','
                     updateModel $ UpdateNextSelected sel
                   190 -> do
                     updateModel $ UpdateCell sel Off -- '.'
                     updateModel $ UpdateNextSelected sel
                   h'' -> if isAlpha $ chr h''
                            then do
                              updateModel $ UpdateCell sel (Alpha . toUpper $ chr h'')
                              updateModel $ UpdateNextSelected sel
                            else pure ()
                 else pure ()
        _ -> pure ()

