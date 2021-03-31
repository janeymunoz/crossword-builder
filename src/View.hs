{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module View where

import Protolude
import Control.Lens ((^.))
import Control.Lens.Wrapped (op)
import Data.Char (chr, isAlpha, toUpper)
import Data.Map.Strict as Map hiding (map)
import qualified Data.Maybe as Maybe
import Data.IntMap.Strict as IntMap hiding (map)
import qualified Data.Text as Text
import Miso
import Miso.String (ms)

import Action
import Crossword
import Model

cellSize :: Int
cellSize = 40

gridSize :: Size -> Int
gridSize si = (sizeToInt si) * cellSize

viewModel :: Model -> View Action
viewModel model =
  div_ []
    [ div_
      [ class_ "cells"
      , style_ $
          Map.fromList
            [ ( "display", "grid" )
            , ( "grid-template-columns", cellDims)
            , ( "grid-template-rows", cellDims)
            ]
      , onDoubleClick UpdateDirection
      ]
      $ map (cell si cs dir hi sel answerIds $ model ^. gridSelected) $ Map.toList cs
    , vSymmetry sym
    , vSize si
--    , vSave
    , vWords words'
    , div_
      [ class_ "model" ]
      [ p_ [] [text . ms $ (show model :: Text) ] ]
    ]
  where
    cellDims = ms . intercalate " " . replicate (sizeToInt si) $ mconcat [show cellSize, "px"]
    b = model^.board
    si = b^.Crossword.size
    cs = b^.cells
    dir = b^.direction
    hi = b^.highlighted
    sel = b^.selected
    answerIds = getAnswerIDs si cs
    sym = b ^. symmetry
    words' = b ^. wordBank

cell :: Size -> Cells -> Direction -> Maybe (CellID, CellID) -> CellID -> IntMap.IntMap CellID -> Bool -> (CellID, CellState) -> View Action
cell si cs dir highlighted selected answerIds gridSelected (currentCellID, currentCellState) =
  div_
    [ class_ "cell"
    , style_ $
        Map.fromList
          [ ("background-color", background)
          , ("color", "black")
          , ("text-align", "center")
          , ("font-family", "Courier New, Courier, monospace")
          , ("border", "1px solid black")
          , ("box-sizing" , "border-box")
          , ("line-height", ms (show cellSize <> "px" :: Text))
          , ("border-collapse", "collapse")
          , ("font-weight", "600")
          ]
    , onClick $ UpdateSelected currentCellID
    ]
    valueCell
  where
    cellSize' = ms (show cellSize :: Text)
    valueCell = case currentCellState of
      Alpha c -> [ text $ ms [c] ]
      _ -> []
    background
      | currentCellID == selected = selected'
      | gridSelected && (fromMaybe False . fmap (elem currentCellID) $ range <$> highlighted) = highlighted'
      | otherwise = case currentCellState of
          Empty -> empty'
          Off -> black
          _ -> white
    getAnswerId :: IntMap.IntMap CellID -> CellID -> Maybe Int
    getAnswerId answerIds cId = Maybe.listToMaybe . Maybe.catMaybes . map (\(k,v) -> if v == cId then Just k else Nothing) $ IntMap.toList answerIds

vSymmetry :: Symmetry -> View Action
vSymmetry sym =
  div_
    [ class_ "buttons-symmetry"
    , style_ $ Map.fromList [("display", "flex")]
    ]
    $ map button [XY ..]
  where
    button :: Symmetry -> View Action
    button sym' =
      div_
        [ class_ "button"
        , style_ $ Map.fromList
            [ ("background-color", if sym' == sym then empty' else "white")
            , ("color", "black")
            , ("text-align", "center")
            , ("font-family", "Courier New, Courier, monospace")
            , ("border", "1px solid black")
            , ("box-sizing" , "border-box")
            , ("line-height", ms (show cellSize <> "px" :: Text))
            , ("border-collapse", "collapse")
            , ("font-weight", "600")
            , ("float", "right")
            , ("width", "80px")
            ]
        , onClick $ UpdateSymmetry sym'
        ]
        [ text . ms $ (show sym' :: Text) ]

vSize :: Size -> View Action
vSize size =
  div_
    [ class_ "buttons-size"
    , style_ $ Map.fromList [("display", "flex")]
    ]
    $ map button [Fifteen, TwentyOne]
  where
    button :: Size -> View Action
    button size' =
      div_
        [ class_ "button"
        , style_ $ Map.fromList
            [ ("background-color", if size' == size then empty' else "white")
            , ("color", "black")
            , ("text-align", "center")
            , ("font-family", "Courier New, Courier, monospace")
            , ("border", "1px solid black")
            , ("box-sizing" , "border-box")
            , ("line-height", ms (show cellSize <> "px" :: Text))
            , ("border-collapse", "collapse")
            , ("font-weight", "600")
            , ("float", "right")
            , ("width", "100px")
            ]
        , onClick $ UpdateSize size'
        ]
        [ text . ms $ (show size' :: Text) ]

vWords :: [Word'] -> View Action
vWords words =
  div_
    [ class_ "words" ]
    [ div_
        []
        [ h2_ [] [ text "Stats" ]
        , p_ [] [ text . ms $ mconcat ["Number of words: ", show $ length words :: Text ] ]
        ]
    , div_
        [ class_ "words-across"
        , style_ $ Map.fromList [("display", "block") ]
        ]
        $ mconcat
          [ [ h2_ [] [ text "Across" ] ]
          , acrosses
          ]
    , div_
        [ class_ "words-down"
        , style_ $ Map.fromList [("display", "block") ]
        ]
        $ mconcat
          [ [ h2_ [] [ text "Down" ] ]
          , downs
          ]
    ]
  where
    (acrosses, downs) = Protolude.foldr f ([], []) words
    f :: Word' -> ([View Action], [View Action]) -> ([View Action], [View Action])
    f word@(Word' _ _ dir _) (as, ds) =
      case dir of
        Across -> (x word : as, ds)
        _ -> (as, x word : ds)
      where
        x :: Word' -> View Action
        x (Word' _ ansId _ val) =
          div_
            [ style_ $ Map.fromList
              [ ("display", "grid")
              , ("grid-template-columns","max-content max-content")
              , ("grid-gap","5px")
              ]
            ]
            [ label_
                [ Miso.for_ "clue"
                , style_ $ Map.fromList [("text-align", "right")]
                ]
                [ text . ms $ mconcat [ show ansId, " ", Text.pack val] ]
            , input_
                [ type_ "text"
                , id_ "clue"
                ]
            ]

vSave :: View Action
vSave =
  div_
    [ class_ "button"
    , style_ $ Map.fromList
        [ ("background-color", "white")
        , ("color", "black")
        , ("text-align", "center")
        , ("font-family", "Courier New, Courier, monospace")
        , ("border", "1px solid black")
        , ("box-sizing" , "border-box")
        , ("line-height", ms (show cellSize <> "px" :: Text))
        , ("border-collapse", "collapse")
        , ("font-weight", "600")
        , ("width", "80px")
        ]
--    , onClick Save
    ]
    [ text "save" ]

selected' = "rgba(60, 210, 195, 1)"
white = "rgba(255, 255, 255, 1)"
black = "rgba(0, 0, 0, 1)"
empty' = "rgba(0, 0, 0, 0.1)"
highlighted' = "rgba(60, 210, 195, 0.4)"

