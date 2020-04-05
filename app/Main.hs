{-# LANGUAGE RecordWildCards #-}

module Main where

import Protolude
import qualified Miso

import qualified Action
import qualified Crossword
import qualified Model
import qualified Update
import qualified View

main :: IO ()
main =
  Miso.startApp $
    Miso.App
       { Miso.initialAction = Action.NoAction
       , Miso.model         = Model.Model $ Crossword.startBoard Crossword.Fifteen "test.cw"
       , Miso.update        = Miso.fromTransition . Update.updateModel
       , Miso.view          = View.viewModel
       , Miso.events        = Miso.defaultEvents
       , Miso.mountPoint    = Nothing
       , Miso.subs          = []
       }
