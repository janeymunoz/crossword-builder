{-# LANGUAGE RecordWildCards #-}

module Main where

import Protolude
import qualified Graphics.Gloss as GG
import qualified Graphics.Gloss.Interface.IO.Interact as GG
import qualified Data.IntMap as IntMap
import qualified Data.Char as DC
import qualified Data.Text as DT
import qualified Crossword as CW


main :: IO ()
main = 
  GG.interactIO
    (GG.InWindow "Crossword Builder" (displaySize CW.Fifteen) (100,100))
    GG.white
    (CW.startBoard CW.Fifteen)
    boardToPic
    eventCallback
    (const $ pure ())

--------------------------------------------------------------------------------
-- Converting Crossword features to Gloss data types
--------------------------------------------------------------------------------

--------------------
-- Dimensions of GUI
--------------------

type Px = Int

-- Arbitrary pixel length of one of the sides of a cell within the grid.
-- The other features are 'built off' of this value.
cellSize :: Px
cellSize = 30

-- Arbitrary width of panel that will house several buttons
panelSizeW :: Px
panelSizeW = 200

-- Arbitrary width of a border around grid
borderW :: Px
borderW = 30

-- Grid is a square. The output is the length of a square edge in pixels.
gridSize :: CW.Size -> Px
gridSize s = cellSize * (CW.sizeToInt s)

-- Size of the display formatted for Gloss
displaySize :: CW.Size -> (Px, Px)
displaySize s = (l + panelSizeW + borderW, l + borderW + borderW)
  where l = gridSize s

------------------
-- Location in GUI
------------------

-- Given a crossword Size and any CellID, will output the coordinate path of a
-- the corresponding square within a grid.
cellCoords :: CW.Size -> CW.CellID -> [GG.Point]
cellCoords s (CW.CellID r c) = [ (xStart, yStart)
                               , (xStart, yStart - cS)
                               , (xStart + cS, yStart - cS)
                               , (xStart + cS, yStart)
                               ]
  where offCenter = div (gridSize s) 2
        cS = toEnum cellSize
        xStart = toEnum $ (c - 1) * cellSize - offCenter - (div panelSizeW 2) + borderW
        yStart = toEnum $ offCenter - (r - 1) * cellSize

-- Given a crossword Size and a list of CellIDs, will output a list of all
-- coordinate paths for the corresponding squares within a grid.
allCellCoords :: CW.Size -> [CW.CellID] -> [[GG.Point]]
allCellCoords s cIDs = map (cellCoords s) cIDs

xyToCellID :: CW.Size -> (Float, Float) -> CW.CellID
xyToCellID si (x,y) =
  CW.CellID (fromEnum ((offCenter + cellSizeF - y) / cellSizeF))
            (fromEnum (((toEnum $ div panelSizeW 2) - (toEnum borderW)
              + x + offCenter + cellSizeF) / cellSizeF))
  where offCenter = toEnum $ div (gridSize si) 2
        cellSizeF = toEnum cellSize

------------------------------------
-- Crossword data as a Gloss Picture
------------------------------------

-- Transforms the state of the board into a picture
boardToPic :: CW.Board -> IO GG.Picture
boardToPic b = pure $ GG.pictures [ csPic, hPic, selPic, grid si, addAnswerIDs si aIDs, button si "load"]
  where csPic = GG.pictures . map parsePics $ cellsToPic si cs $ CW.allCellIDs si
        hPic = highlightedPic si hs
        selPic = GG.color (GG.withAlpha 0.7 GG.cyan) $ GG.polygon $ cellCoords si sel
        sel = CW.selected b
        si = CW.size b
        cs = CW.cells b
        hs = CW.highlighted b
        ws = CW.wordBank b
        aIDs = CW.answerIDs b

-- Adds grid and highlighting to the board Picture
grid :: CW.Size -> GG.Picture
grid s = GG.pictures . map (GG.color GG.black) . map GG.lineLoop . allCellCoords s $ CW.allCellIDs s

parsePics :: Maybe GG.Picture -> GG.Picture
parsePics mP =
  case mP of
    Just p -> p
    Nothing -> GG.Blank

cellsToPic :: CW.Size -> IntMap.IntMap (IntMap.IntMap CW.Cell) -> [CW.CellID] -> [Maybe GG.Picture]
cellsToPic si cs cIDs = map (cellToPic si cs) cIDs

cellToPic :: CW.Size -> IntMap.IntMap (IntMap.IntMap CW.Cell) -> CW.CellID -> Maybe GG.Picture
cellToPic si cs cID =
  case CW.getCellState cs cID of
    Nothing -> Nothing
    Just st -> Just . (addAlpha si st cID) . GG.color (cellColor st) $ GG.polygon (cellCoords si cID)

addAlpha :: CW.Size -> CW.Cell -> CW.CellID -> GG.Picture -> GG.Picture
addAlpha si cState cID p =
  case cState of
    CW.Alpha c -> GG.translate (lowerLX + (toEnum $ div cellSize 4)) (lowerLY + (toEnum $ div cellSize 4)) $ GG.scale (1 / scaler) (1 / scaler) $ GG.pictures [ p, GG.text [ DC.toUpper c] ]
      where scaler = (toEnum cellSize) / 5
            [_, (lowerLX, lowerLY), _, _] = cellCoords si cID
    _          -> p

addAnswerIDs :: CW.Size -> IntMap.IntMap CW.CellID -> GG.Picture
addAnswerIDs si ansIDs = GG.pictures . map (getPic si) $ IntMap.toAscList ansIDs
  where
    getPic :: CW.Size -> (IntMap.Key, CW.CellID) -> GG.Picture
    getPic si (k, cID) = GG.translate transX (transY - 10.0) . GG.scale scaler scaler . GG.text $ show k
      where [(transX, transY), _, _, _] = cellCoords si cID
            scaler :: Float
            scaler = (toEnum cellSize) / 450

highlightedPic :: CW.Size -> Maybe (CW.CellID, CW.CellID) -> GG.Picture
highlightedPic si cIDs =
  case cIDs of
    Nothing -> GG.blank
    Just (CW.CellID r1 c1, CW.CellID r2 c2) ->
      GG.pictures . map (GG.color $ GG.withAlpha 0.4 GG.cyan) $ map GG.polygon allHs
        where allHs = map (cellCoords si) [ CW.CellID r c | r <- [r1..r2], c <- [c1..c2] ]

-- Associates Cell states to Colors
cellColor :: CW.Cell -> GG.Color
cellColor cellState =
  case cellState of
    CW.On      -> GG.white
    CW.Off     -> GG.black
    CW.Empty   -> GG.greyN 0.5
    CW.Alpha _ -> GG.white


----------------------
-- Buttons as pictures
----------------------

-- Arbitrary width (x) and length (y) of a button

buttonSize :: (Px, Px)
buttonSize = (cellSize * 3, cellSize * 2)

data Button = Button [Char]

buttonsAll :: [Button]
buttonsAll = [ Button "load"
             ]

buttonsToPic :: CW.Size -> [Button] -> (Float, Float)
buttonsToPic si buttons = (buttonSiX, buttonSiY)
  where buttonSiX = toEnum $ fst buttonSize
        buttonSiY = toEnum $ snd buttonSize

-- Returns a Picture of a Button that starts in the given CellID
buttonToPic :: CW.Size -> Button -> CW.Row -> GG.Picture
buttonToPic si button r =
  GG.translate transX transY $ GG.color GG.azure $ GG.rectangleSolid buttonSiX buttonSiY
  where buttonSiX = toEnum $ fst buttonSize
        buttonSiY = toEnum $ snd buttonSize
        transX = undefined
        transY = undefined

buttonPos :: CW.Size -> CW.Row -> (Float, Float)
buttonPos si r = (1.0, 1.0)
  where transX = 2 + CW.sizeToInt si

-- cellCoords :: CW.Size -> CW.CellID -> [GG.Point]
button si st =
    GG.translate xTrans yTrans $ 
      GG.pictures 
        [ GG.color GG.azure $ GG.rectangleSolid (toEnum $ fst buttonSize) (toEnum $ snd buttonSize)
        , GG.translate (-25.0) (-10.0) $ GG.scale 0.25 0.25 $ GG.text st 
        ]
  where (xTotal, yTotal) = displaySize si
        xUpperR = toEnum xTotal / 2
        yUpperR = toEnum yTotal / 2
        xTrans = xUpperR - toEnum (3 * borderW)
        yTrans = yUpperR - toEnum (2 * borderW)
        
goButton :: (x,y)
goButton = undefined

--------------------------------------------------------------------------------
-- Event handling
--------------------------------------------------------------------------------

-- Event for selecting cells
handleEvent :: GG.Event -> CW.Board -> CW.Board
handleEvent e b@CW.Board{..} =
  case e of
    -- select new cell with mouse
    GG.EventKey (GG.MouseButton GG.LeftButton) GG.Down _ (x, y)
      -> CW.updSelAndHi b $ xyToCellID size (x, y)
    -- select new cell with arrow keys
    GG.EventKey (GG.SpecialKey GG.KeyUp) GG.Down _ _
      -> CW.updSelAndHi b $ selectWithKey size direction selected GG.KeyUp
    GG.EventKey (GG.SpecialKey GG.KeyDown) GG.Down _ _
      -> CW.updSelAndHi b $ selectWithKey size direction selected GG.KeyDown
    GG.EventKey (GG.SpecialKey GG.KeyLeft) GG.Down _ _
      -> CW.updSelAndHi b $ selectWithKey size direction selected GG.KeyLeft
    GG.EventKey (GG.SpecialKey GG.KeyRight) GG.Down _ _
      -> CW.updSelAndHi b $ selectWithKey size direction selected GG.KeyRight
    -- switch direction of board with mouse
    GG.EventKey (GG.MouseButton GG.RightButton) GG.Down _ (x, y)
      -> b { CW.direction = CW.otherDir direction
           , CW.highlighted =
               CW.newHighlighted size cells (CW.otherDir direction) selected
           }
    -- switch direction of board with keyboard (tab)
    GG.EventKey (GG.SpecialKey GG.KeyTab) GG.Down _ _
      -> b { CW.direction = CW.otherDir direction
           , CW.highlighted =
               CW.newHighlighted size cells (CW.otherDir direction) selected
           }
    -- switch on/off with mouse
    GG.EventKey (GG.MouseButton GG.MiddleButton) GG.Down _ (x, y)
      -> (CW.updSelAndHi b selectedCID) 
           { CW.cells = newCells
           , CW.answerIDs = CW.getAnswerIDs size newCells
           , CW.wordBank = CW.makeWords size newCells
           }
         where selectedCID = xyToCellID size (x, y)
               newCells = 
                 CW.updateCellAndComp size symmetry selectedCID
                   (CW.toggleCellState $ CW.getCellState cells selectedCID)
                   cells
    -- switch on/off with keyboard (space)
    GG.EventKey (GG.SpecialKey GG.KeySpace) GG.Down _ _
      -> (CW.updSelAndHi b selected)
           { CW.cells = newCells
           , CW.answerIDs = CW.getAnswerIDs size newCells
           , CW.wordBank = CW.makeWords size newCells
           }
         where newCells = 
                 CW.updateCellAndComp size symmetry selected
                   (CW.toggleCellState $ CW.getCellState cells selected) cells
    -- update cell to a character
    GG.EventKey (GG.Char c) GG.Down _ _
      -> b { CW.cells = newCells
           , CW.answerIDs = CW.getAnswerIDs size newCells
           , CW.selected = CW.nextSelected size direction selected cells
           , CW.wordBank = CW.makeWords size newCells
           }
         where newCells =
                 CW.updateCellAndComp size symmetry selected (CW.Alpha c) cells
    _ -> b


-----------------------------------
-- Supplementary for event handling
-----------------------------------

-- Saves the board after every event
eventCallback :: GG.Event -> CW.Board -> IO CW.Board
eventCallback event board = do
  let newBoard = handleEvent event board
      (across, down) = CW.parseWords (CW.wordBank newBoard) ([], [])
  writeFile ".board" . DT.pack $ show newBoard
  writeFile ".words" . DT.unlines $
    ((DT.pack "Across") : across) ++
        ((DT.pack "\nDown") : down)
  pure newBoard

selectWithKey :: CW.Size -> CW.Direction -> CW.CellID -> GG.SpecialKey -> CW.CellID
selectWithKey si dir (CW.CellID curR curC) k =
  case k of
    GG.KeyUp    -> CW.ifEndBegin si $ CW.CellID (curR-1) curC
    GG.KeyDown  -> CW.ifEndBegin si $ CW.CellID (curR+1) curC
    GG.KeyLeft  -> CW.ifEndBegin si $ CW.CellID curR (curC-1)
    GG.KeyRight -> CW.ifEndBegin si $ CW.CellID curR (curC+1)
    _           -> CW.CellID curR curC
