{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Crossword where

import Protolude hiding (Down)
import qualified Control.Lens as Lens
import qualified Control.Lens.At as At
import qualified Control.Lens.Operators as Ops
import qualified Data.Char as DC
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.List as DL
import qualified Data.Set as DS
import qualified Data.Text as DT hiding (zip, replicate)

--------------------------------------------------------------------------------
-- Definitions
--------------------------------------------------------------------------------

type Row = Int
type Col = Int

data CellID = CellID Row Col
  deriving (Eq, Ord, Read, Show)

data CellState
  = On
  | Off
  | Empty
  | Alpha Char
  deriving (Eq, Read, Show)

type AnswerID = Int

data Direction
  = Across
  | Down
  deriving (Show, Read, Eq)

data Word' = Word' CellID AnswerID Direction [Char]
  deriving (Show, Read, Eq)

-- Map Row (Map Col CellState)
type Cells' = IntMap.IntMap (IntMap.IntMap CellState)
type Cells = Map CellID CellState

data Rel
  = Plus
  | Minus
  deriving (Show, Read, Eq)

data Size
  = Fifteen
  | TwentyOne
  deriving (Show, Read, Eq)

data Symmetry
  = XY
  | X
  | Y
  | NoSym
  deriving (Show, Read, Eq)

data Board = Board
  { _direction :: Direction
  , _size :: Size
  , _symmetry :: Symmetry
  , _selected :: CellID
  , _highlighted :: Maybe (CellID, CellID)
  , _answerIDs :: IntMap.IntMap CellID
  , _wordBank :: [Maybe Word']
  , _cells :: Cells
  , _sessionID :: FilePath
  }
  deriving (Show, Read, Eq)

direction :: Lens.Lens' Board Direction
direction = Lens.lens _direction $ \board newDir -> board { _direction = newDir }

size :: Lens.Lens' Board Size
size = Lens.lens _size $ \board newSize -> board { _size = newSize }

symmetry :: Lens.Lens' Board Symmetry
symmetry = Lens.lens _symmetry $ \board newSym -> board { _symmetry = newSym }

selected :: Lens.Lens' Board CellID
selected = Lens.lens _selected $ \board newSel -> board { _selected = newSel }

highlighted :: Lens.Lens' Board (Maybe (CellID, CellID))
highlighted = Lens.lens _highlighted $ \board newCIDs -> board { _highlighted = newCIDs }

answerIDs :: Lens.Lens' Board (IntMap.IntMap CellID)
answerIDs = Lens.lens _answerIDs $ \board newMapCIDs -> board { _answerIDs = newMapCIDs }

wordBank :: Lens.Lens' Board [Maybe Word']
wordBank = Lens.lens _wordBank $ \board newWordBank -> board { _wordBank = newWordBank }

cells :: Lens.Lens' Board Cells
cells = Lens.lens _cells $ \board newCells -> board { _cells = newCells }

sessionID :: Lens.Lens' Board FilePath
sessionID = Lens.lens _sessionID $ \board newSID -> board { _sessionID = newSID }

-------------------------------
-- Supplementary
-------------------------------

sizeToInt :: Size -> Int
sizeToInt size =
  case size of
    Fifteen   -> 15
    TwentyOne -> 21

-- Given a crossword Size, will output all CellIDs
allCellIDs :: Size -> [CellID]
allCellIDs size = [ CellID r c | r <- range, c <- range ]
  where range = [ 1..sizeToInt size ]

--------------------------------------------------------------------------------
-- Initial Crossword state
--------------------------------------------------------------------------------

startBoard :: Size -> FilePath -> Board
startBoard si fp =
  Board { _direction = Across
        , _size = si
        , _symmetry = XY
        , _selected = CellID 1 1
        , _highlighted = Just (CellID 1 1, CellID 1 numCells)
        , _answerIDs = getAnswerIDs si $ startCells si
        , _wordBank = []
        , _cells = startCells si
        , _sessionID = fp
        }
    where numCells = sizeToInt si

startCells :: Size -> Cells
startCells si = Map.fromAscList . zip (allCellIDs si) $ repeat Empty

--------------------------------------------------------------------------------
-- Updating the Crossword
--------------------------------------------------------------------------------

-- Updates the state of a selected cell.
updateCell :: CellID -> CellState -> Cells -> Cells
updateCell = Map.insert

-- Updates the state of a selected cell and its complement.
updateCellAndComp :: Size -> Symmetry -> CellID -> CellState -> Cells -> Cells
updateCellAndComp si sym cMain cStateNew cs =
  updateCell cComp cCompNew cMainUpdated
    where
      cMainUpdated = updateCell cMain cStateNew cs
      cComp = case complementCellID si sym cMain of
                Nothing  -> cMain
                Just cID -> cID
      cCompOld = case getCellState cs cComp of
                   Nothing -> cStateNew
                   Just s -> s
      cCompNew = updateComp cStateNew cCompOld

updSelAndHi :: Board -> CellID -> Board
updSelAndHi b@Board{..} selectedCID =
  b { _selected = case inRange _size selectedCID of
                   True  -> selectedCID
                   False -> _selected
    , _highlighted = newHighlighted _size _cells _direction selectedCID
    }

inRange :: Size -> CellID -> Bool
inRange si (CellID r c) =
  case r > maxCell || c > maxCell || r * c == 0 of
    True -> False
    False -> True
  where maxCell = sizeToInt si

---------------------
-- Updating Word Bank
---------------------

parseWords :: [Maybe Word'] -> ([DT.Text], [DT.Text]) -> ([DT.Text], [DT.Text])
parseWords wordMs (across, down) =
  case wordMs of
    [] -> (across, down)
    Nothing:ws -> parseWords ws (across, down)
    Just (Word' cID ansID Across chars) : ws -> parseWords ws (across ++ [DT.pack ((show ansID) ++ " " ++ chars)], down)
    Just (Word' cID ansID Down chars) : ws -> parseWords ws (across, down ++ [DT.pack ((show ansID) ++ " " ++ chars)])

makeWords :: Size -> Cells -> [Maybe Word']
makeWords si cs =
  Protolude.map (toWord si cs Across) across ++
    Protolude.map (toWord si cs Down) down
  where
    across = DS.toList $ getWordEnds si cs Across
    down   = DS.toList $ getWordEnds si cs Down

toWord :: Size
       -> Cells
       -> Direction
       -> Maybe (CellID, CellID)
       -> Maybe Word'
toWord si cs dir range =
  case range of
    Nothing -> Nothing
    Just (cID, _) -> Just $ Word' cID ansID dir chars
      where
        chars =
          getWordStr . getWordChars $ getCellStateWord dir cs range
        [(ansID, _)] =
          IntMap.toList . IntMap.filter ((==) cID) $ getAnswerIDs si cs

getWords :: Size
         -> Cells
         -> Direction
         -> [DT.Text]
getWords si cs dir =
  Protolude.map DT.pack .
    Protolude.map getWordStr .
      Protolude.map getWordChars .
        Protolude.map (getCellStateWord dir cs) .
          DS.toList $ getWordEnds si cs dir

mapCellToChar :: Maybe CellState -> Maybe Char
mapCellToChar cState =
  case cState of
    Nothing        -> Nothing
    Just On        -> Just '_'
    Just Off       -> Nothing
    Just Empty     -> Just '_'
    Just (Alpha c) -> Just c

getWordStr :: [Maybe Char] -> [Char]
getWordStr csMaybe =
  case csMaybe of
    (Nothing:cs)  -> getWordStr cs
    ((Just c):cs) -> c : getWordStr cs
    []            -> []

getWordChars :: Maybe [Maybe CellState] -> [Maybe Char]
getWordChars cellGroup =
  case cellGroup of
    Nothing -> []
    Just cs -> Protolude.map mapCellToChar cs

getCellStateWord :: Direction
                 -> Cells
                 -> Maybe (CellID, CellID)
                 -> Maybe [Maybe CellState]
getCellStateWord dir cs ends = case ends of
  Nothing -> Nothing
  Just es ->
    Just . Protolude.map (getCellState cs) $ range es

range :: (CellID, CellID) -> [ CellID ]
range (CellID r0 c0, CellID r1 c1) = [ CellID r c | r <- [r0..r1], c <- [c0..c1] ]
      
getWordEnds :: Size
         -> Cells
         -> Direction
         -> DS.Set (Maybe (CellID, CellID))
getWordEnds si cs dir =
  DS.fromList $ Protolude.map (newHighlighted si cs dir) $ allCellIDs si

---------------------------------
-- Associating words with numbers
---------------------------------

-- Down direction takes precedence
-- If cell of row or col is at an edge and cell is On, cells should have an AnswerID
-- An 'edge' is the start of the grid itself, or an Off cell.
-- data Word' = Word' CellID Direction [Char]

getAnswerIDs :: Size -> Cells -> IntMap.IntMap CellID
getAnswerIDs si cs = labelEdges $ getEdgeIDs si cs

labelEdges :: DS.Set CellID -> IntMap.IntMap CellID
labelEdges edges = IntMap.fromAscList . zip [1..numEdges] $ DS.toAscList edges
  where numEdges = DS.size edges

getEdgeIDs :: Size -> Cells -> DS.Set CellID
getEdgeIDs si cs =
  DS.union (getEdgeIDsDir si cs Down) (getEdgeIDsDir si cs Across)

getEdgeIDsDir :: Size -> Cells -> Direction -> DS.Set CellID
getEdgeIDsDir si cs dir =
  DS.fromList . fst . DL.partition (isEdge cs dir) $ allCellIDs si

isEdge :: Cells -> Direction -> CellID -> Bool
isEdge cs dir cID =
  case getCellState cs cID of
    Nothing     -> False
    Just cState -> case cState of
                     Off -> False
                     _   -> case getCellState cs $ adjacentCellID dir Minus cID of
                              Nothing  -> True
                              Just Off -> True
                              _        -> False

-------------------------------------------
-- Supplementary for updating the Crossword
-------------------------------------------

-- Returns a tuple representing an inclusive range over which the cells 
-- in the grid should be highlighted.
newHighlighted :: Size
               -> Cells
               -> Direction
               -> CellID
               -> Maybe (CellID, CellID)
newHighlighted si cs dir selectedCID =
  case lookUntil si cs selectedCID dir Minus selectedCID of
    Nothing -> Nothing
    Just cIDM -> case lookUntil si cs selectedCID dir Plus selectedCID of
                   Nothing -> Nothing
                   Just cIDP -> Just (cIDM, cIDP)

-- 'Looks' a relative direction until a cell is Off or a wall is hit.
lookUntil :: Size
          -> Cells
          -> CellID
          -> Direction
          -> Rel
          -> CellID
          -> Maybe CellID
lookUntil si cs selectedCID dir dirRel curCell =
  -- if selected cell is Off, nothing highlighted
  case inRange si selectedCID of
    True -> case getCellState cs selectedCID of
              Just Off -> Nothing
              _        -> case getCellState cs adjCID of
                            Nothing  -> Just curCell
                            Just Off -> Just curCell
                            _        -> lookUntil si cs selectedCID dir dirRel adjCID
            where adjCID = adjacentCellID dir dirRel curCell
    False -> Nothing

-- Returns the CellID of a cell adjacent to to a given cell.
adjacentCellID :: Direction -> Rel -> CellID -> CellID
adjacentCellID dir dirRel (CellID row col) = case dir of
  Across -> CellID row $ col + dirVal
  Down   -> CellID (row + dirVal) col
  where
    dirVal = case dirRel of
      Plus  -> 1
      Minus -> -1

-- Returns a cell's state from the group of all cells.
getCellState :: Cells -> CellID -> Maybe CellState
getCellState cs cID = Map.lookup cID cs

-- If a cell's state is updated, its complement will also need to be updated.
-- This function outlines how the complement should be updated.
updateComp :: CellState -> CellState -> CellState
updateComp changedCell compCell = case changedCell of
  Off -> Off
  Empty -> Empty
  On -> case compCell of
          Alpha c -> Alpha c
          _       -> On
  Alpha c -> case compCell of
               Alpha d -> Alpha d
               _       -> On

-- Returns a cell's complement CellID.
complementCellID :: Size -> Symmetry -> CellID -> Maybe CellID
complementCellID si sym (CellID row col) =
  case sym of
    XY -> Just $ CellID (numCells - row + 1) (numCells - col + 1)
    X -> Just $ CellID (numCells - row + 1) col
    Y -> Just $ CellID row (numCells - col + 1)
    NoSym -> Nothing
  where numCells = sizeToInt si

-- If an adjacent CellID is outside of the board size, will return a CellID as
-- if cells are in a continuous loop.
ifEndBegin :: Size -> CellID -> CellID
ifEndBegin si (CellID r c)
  | r < 1     = CellID siInt c
  | r > siInt = CellID 1 c
  | c < 1     = CellID r siInt
  | c > siInt = CellID r 1
  | otherwise = CellID r c
  where siInt = sizeToInt si

-- Retruns the CellID of the next cell that should be selected after the
-- currently selected cell.
nextSelected :: Size -> Direction -> CellID -> Cells -> CellID
nextSelected si dir selectedCID cs =
  case rAdj > numCells || cAdj > numCells of
    True -> case lookUntil si cs selectedCID dir Minus selectedCID of
              Nothing    -> selectedCID
              Just cAdjL -> cAdjL
    False -> CellID rAdj cAdj
  where (CellID rAdj cAdj) = adjacentCellID dir Plus selectedCID
        numCells = sizeToInt si

-- Given a direction, returns the other direction.
otherDir :: Direction -> Direction
otherDir d = case d of
  Down   -> Across
  Across -> Down

-- Given a cell's state, returns the next cell state in the toggling series.
toggleCellState :: Maybe CellState -> CellState
toggleCellState cState = case cState of
  Nothing -> Empty
  Just cS -> case cS of
    Empty -> Off
    Off -> On
    On -> Empty
    Alpha _ -> Off

