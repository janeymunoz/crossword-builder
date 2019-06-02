module Main where

import Protolude
import Crossword

main :: IO ()
main = do
  putStrLn "Welcome to the CROSSWORD BUILDER 3000"
  putStrLn "Enter the size of the crossword to be built (Fifteen of TwentyOne)."
  s <- getInput "" :: IO Size
  putStrLn "Enter the symmetry of the crossword to be built (X, Y, XY, or NoSym)."
  sy <- getInput "" :: IO Symmetry
  let b = Board { direction = Across
                , size = s
                , symmetry = sy
                , selected = CellID 1 1
                , highlighted = Just ((CellID 1 1), (CellID 1 $ sizeToInt s))
                , answerIDs = labelEdges . getEdgeIDs s $ startCells s
                , wordBank = []
                , cells = startCells s
                , sessionID = []
                }
  loop b
  
loop :: Board -> IO ()
loop b = do
  let s = size b
      cs = cells b
  putStrLn $ prettyBoard b
  putStrLn "Enter the row and column number of a cell you'd like to select."
  putStr "Row: "
  row <- getInt s
  putStr "Column: "
  col <- getInt s
  putStrLn "Enter the update value for the cell (On, Off, Empty, Alpha CHAR)."
  cState <- getInput "" :: IO Cell
  let newMain = updateCell (CellID row col) cState cs
      compID = complementCellID (size b) (symmetry b) (CellID row col)
      newComp = case compID of
                  Nothing  -> newMain
                  Just cID ->
                    case getCellState newMain cID of
                      Nothing -> newMain
                      Just cStateOld ->
                        updateCell cID (updateComp cState cStateOld) newMain
  loop $ b { cells = newComp}
  return ()

--------------------------------------------------------------------------------
-- Helper functions for parsing and validating user input
--------------------------------------------------------------------------------

getInput :: Read a => [Char] -> IO a
getInput s = do
  c <- getLine
  case readMaybe $ toS c of
    Nothing -> do
      putStrLn "Invalid input, try again."
      getInput s
    Just c -> return c

getInt :: Size -> IO Int
getInt s = do
  i <- getLine
  case readMaybe $ toS i of
    Nothing -> do
      putStrLn "Invalid input, try again."
      getInt s
    Just i -> case ruleRowCol s i of
                True  -> return i
                False -> do
                  putStrLn "Invalid input, try again."
                  getInt s

ruleRowCol :: Size -> Int -> Bool
ruleRowCol s i =
   case s of
     Fifteen   -> i <= 15
     TwentyOne -> i <= 21
