{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Model where

import Protolude
import qualified Control.Lens as Lens
import System.FilePath as FP

import qualified Crossword

data Model = Model
  { _board :: Crossword.Board
--  , _sessionID :: FP.FilePath
  }
  deriving (Eq, Show)

board :: Lens.Lens' Model Crossword.Board
board = Lens.lens _board $ \model newBoard -> model { _board = newBoard }
