module Logic where


import Graphics.Gloss.Interface.IO.Game
    ( Key(Char, MouseButton, SpecialKey),
      Event(EventKey),
      KeyState(Down),
      MouseButton(LeftButton),
      SpecialKey(KeySpace) )
import Game
    ( SudokuGame(SudokuGame, grid, finished, selectedCell),
      gridSize,
      cellWidth,
      cellHeight,
      middleOfGridX,
      middleOfGridY,
      isCellValid,
      updateGrid,
      isInitialValue,
      initialGame,
      checkFinishedGrid )

import Data.Char (ord, chr)
import Solver (constraintSudokuSolver)



handleInput :: Event -> SudokuGame -> SudokuGame
handleInput (EventKey (MouseButton LeftButton) Down _ mouse) game = case getClickedCell mouse of
                                                                    Just cell -> game { selectedCell = Just cell }
                                                                    Nothing   -> game { selectedCell = Nothing }

-- Prenchimento da celula
handleInput (EventKey (Char c) Down _ _) game@SudokuGame{ selectedCell = Just (x, y), grid = oldGrid }
  | c >= '1' && c <= chr (ord '0' + gridSize)
    && isCellValid (x, y) (read [c]) oldGrid
    && not (isInitialValue (x,y) game)       =
      game { grid = updateGrid (x, y) (read [c]) oldGrid,
             selectedCell = Nothing,
             finished = checkFinishedGrid (updateGrid (x, y) (read [c]) oldGrid) }

-- Apagar valor anterior, só toma caso tenha uma selectedCell = Just
handleInput (EventKey (SpecialKey KeySpace) Down _ _) game@SudokuGame{ selectedCell = Just (x, y) }
  | not $ isInitialValue (x,y) game =
      game { grid = updateGrid (x, y) 0 (grid game), selectedCell = Nothing }
      
-- Tip
handleInput (EventKey (Char 't') Down _ _) game@SudokuGame{selectedCell = Just (x, y)} = 
  let solution = constraintSudokuSolver initialGame
      newGame =
        case solution of
          Just sol -> game { grid = updateGrid (x, y) (sol !! x !! y) (grid game) }
          Nothing  -> game   -- TODO RETORNAR O GAME COM UMA MENSAGEM DE ERRO
  in newGame
      

-- reset
handleInput (EventKey (Char 'r') Down _ _) _ = initialGame
handleInput _ game = game

      
    

getClickedCell :: (Float, Float) -> Maybe (Int, Int)
getClickedCell (x, y)
  |    x' < 0 || x' >= fromIntegral gridSize
    || y' < 0 || y' >= fromIntegral gridSize = Nothing
  | otherwise = Just (floor x', floor y')
  where
    x' = (x + (middleOfGridX + cellWidth/2)) / cellWidth
    y' = (y - (middleOfGridY + cellHeight/2)) / (-cellHeight)



