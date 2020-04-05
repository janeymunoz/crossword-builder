{-# LANGUAGE RecordWildCards #-}

module Update where

import Protolude
import qualified Miso

import qualified Action
import qualified Crossword
import qualified Model

updateModel :: Action.Action -> Miso.Transition Action.Action Model.Model ()
updateModel action =
  case action of
    Action.NoAction -> pure ()

