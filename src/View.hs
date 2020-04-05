{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module View where

import Protolude
import qualified Miso

import qualified Action
import qualified Crossword
import qualified Model

viewModel :: Model.Model -> Miso.View Action.Action
viewModel model =
  Miso.div_
    []
    []

