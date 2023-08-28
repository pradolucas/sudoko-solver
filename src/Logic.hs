module Logic where


import Graphics.Gloss.Interface.IO.Game
    ( Key(Char, MouseButton, SpecialKey),
      Event(EventKey),
      KeyState(Down),
      MouseButton(LeftButton),
      SpecialKey(KeySpace) )
import Game
    ( SudokuGame(..),
      gridSize,
      cellWidth,
      cellHeight,
      middleOfGridX,
      middleOfGridY,
      isCellValid,
      updateGrid,
      isInitialValue,
      initialGame,
      checkFinishedGrid, updateCandidates, candidatesFromGrid, resetCandidates )

import Data.Char (ord, chr)
import Solver (constraintSudokuSolver)



handleInput :: Event -> SudokuGame -> SudokuGame
handleInput (EventKey (MouseButton LeftButton) Down _ mouse) game =
  case getClickedCell mouse of
    Just cell -> game { selectedCell = Just cell }
    Nothing   -> game { selectedCell = Nothing }

handleInput (EventKey (Char c) Down _ _) game@SudokuGame{ selectedCell = Just (x, y), grid = oldGrid }
  | c >= '1' && c <= chr (ord '0' + gridSize)
    && isCellValid (x, y) (read [c]) oldGrid
    && not (isInitialValue (x, y) game) =
      game { grid = updateGrid (x, y) (read [c]) oldGrid,
             selectedCell = Nothing,
             finished = checkFinishedGrid (updateGrid (x, y) (read [c]) oldGrid) }

handleInput (EventKey (SpecialKey KeySpace) Down _ _) game@SudokuGame{ selectedCell = Just (x, y), candidates = oldCandidates }
  | not $ isInitialValue (x, y) game =
      game { grid = updateGrid (x, y) 0 (grid game),
             candidates = updateCandidates (x,y) 0 oldCandidates, 
            --  candidates = resetCandidates (x, y) oldCandidates,
             selectedCell = Nothing }

handleInput (EventKey (Char 't') Down _ _) game@SudokuGame{ selectedCell = Just (x, y), grid = oldGrid } =
  let solution = constraintSudokuSolver game
      newGame =
        case solution of
          Just sol -> game { grid = updateGrid (x, y) (sol !! x !! y) oldGrid,
                             selectedCell = Nothing,
                             finished = checkFinishedGrid (updateGrid (x, y) (sol !! x !! y) oldGrid) }
          Nothing  -> game { selectedCell = Nothing }
  in newGame

handleInput (EventKey (Char 'f') Down _ _) game@SudokuGame{ grid = oldGrid, candidates = oldCandidates} =
  let solution = constraintSudokuSolver game
      newGame =
        case solution of
          Just sol -> game { grid = sol,
                             candidates   = candidatesFromGrid oldCandidates oldGrid,
                             selectedCell = Nothing,
                             finished     = checkFinishedGrid sol}
          Nothing  -> game  {selectedCell = Nothing} -- TODO RETORNAR O GAME COM UMA MENSAGEM DE ERRO
  in newGame

handleInput (EventKey (Char 'r') Down _ _) _ = initialGame

-- Start the game when 'e' key is pressed
handleInput (EventKey (Char 'e') Down _ _) game =
  if menuActive game
    then game { menuActive = False }
    else game
-- Start the game when 'e' key is pressed
handleInput (EventKey (Char 'm') Down _ _) game =
  if menuActive game
    then game { menuActive = False }
    else game
-- Start the game when 'e' key is pressed
handleInput (EventKey (Char 'h') Down _ _) game =
  if menuActive game
    then game { menuActive = False }
    else game

-- Default case for other inputs
handleInput _ game = game


getClickedCell :: (Float, Float) -> Maybe (Int, Int)
getClickedCell (x, y)
  |    x' < 0 || x' >= fromIntegral gridSize
    || y' < 0 || y' >= fromIntegral gridSize = Nothing
  | otherwise = Just (floor x', floor y')
  where
    x' = (x + (middleOfGridX + cellWidth/2)) / cellWidth
    y' = (y - (middleOfGridY + cellHeight/2)) / (-cellHeight)



