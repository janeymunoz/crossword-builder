{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Main where

import Protolude
import Miso

import Action
import Crossword
import Model
import Update
import View

main :: IO ()
main = do
  Miso.startApp $
    Miso.App
       { initialAction = NoAction
       , model         = Model (startBoard Fifteen "test.cw") True
       , update        = fromTransition . updateModel
       , view          = viewModel
       , events        = defaultEvents
       , mountPoint    = Nothing
       , subs          = [ arrowsSub ArrowPress
                         , keyboardSub KeyboardPress
                         ]
       }
