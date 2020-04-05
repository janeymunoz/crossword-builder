{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Model where

import Protolude
import qualified Control.Lens as Lens

import qualified Crossword

data Model = Model
  { board :: Crossword.Board
  , sessionID :: 
  }
  deriving (Eq, Show)

