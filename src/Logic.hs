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
handleInput (EventKey (MouseButton LeftButton) Down _ mouse) game = case getClickedCell mouse of
                                                                    Just cell -> game { selectedCell = Just cell }
                                                                    Nothing   -> game { selectedCell = Nothing }

-- Prenchimento da celula
handleInput (EventKey (Char c) Down _ _) game@SudokuGame{ selectedCell = Just (x, y), grid = oldGrid, candidates= oldCandidates }
  | c >= '1' && c <= chr (ord '0' + gridSize)
    && isCellValid (x, y) (read [c]) oldGrid
    && not (isInitialValue (x,y) game)       =
      game { grid = updateGrid (x, y) (read [c]) oldGrid,
             candidates = updateCandidates (x, y) (read [c]) oldCandidates,
             selectedCell = Nothing,
             finished = checkFinishedGrid (updateGrid (x, y) (read [c]) oldGrid) }

-- Apagar valor anterior, só toma caso tenha uma selectedCell = Just
handleInput (EventKey (SpecialKey KeySpace) Down _ _) game@SudokuGame{ selectedCell = Just (x, y), candidates= oldCandidates }
  | not $ isInitialValue (x,y) game =
      game { grid = updateGrid (x, y) 0 (grid game),
             candidates = updateCandidates (x,y) 0 oldCandidates, 
            --  candidates = resetCandidates (x, y) oldCandidates,
             selectedCell = Nothing }
      
-- Tip
handleInput (EventKey (Char 't') Down _ _) game@SudokuGame{selectedCell = Just (x, y),  grid = oldGrid, candidates= oldCandidates} = 
  let solution = constraintSudokuSolver game
      newGame =
        case solution of
          Just sol -> game { grid = updateGrid (x, y) (sol !! x !! y) oldGrid,
                             candidates = updateCandidates (x, y)  (sol !! x !! y) oldCandidates,
                             selectedCell = Nothing,
                             finished = checkFinishedGrid (updateGrid (x, y) (sol !! x !! y) oldGrid)}
          Nothing  -> game  {selectedCell = Nothing} -- TODO RETORNAR O GAME COM UMA MENSAGEM DE ERRO
  in newGame
      
handleInput (EventKey (Char 'f') Down _ _) game@SudokuGame{grid = oldGrid, candidates= oldCandidates} = 
  let solution = constraintSudokuSolver game
      newGame =
        case solution of
          Just sol -> game { grid = sol,
                             candidates   = candidatesFromGrid oldCandidates oldGrid,
                             selectedCell = Nothing,
                             finished     = checkFinishedGrid sol}
          Nothing  -> game  {selectedCell = Nothing} -- TODO RETORNAR O GAME COM UMA MENSAGEM DE ERRO
  in newGame
  
-- Reset
handleInput (EventKey (Char 'r') Down _ _) _ = initialGame

-- everthing else
handleInput _ game = game


getClickedCell :: (Float, Float) -> Maybe (Int, Int)
getClickedCell (x, y)
  |    x' < 0 || x' >= fromIntegral gridSize
    || y' < 0 || y' >= fromIntegral gridSize = Nothing
  | otherwise = Just (floor x', floor y')
  where
    x' = (x + (middleOfGridX + cellWidth/2)) / cellWidth
    y' = (y - (middleOfGridY + cellHeight/2)) / (-cellHeight)



