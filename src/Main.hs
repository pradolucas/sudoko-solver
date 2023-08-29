module Main (main) where

-- import Game
import Game ( initialGame, screenWidth, screenHeight, gridSize)
import Render ( drawGame )
import Logic ( handleInput )

import Graphics.Gloss ( white, play, Display(InWindow) )


window :: Display
window = InWindow ("Sudoku " ++ show gridSize ++ "x" ++ show gridSize) (truncate screenWidth, truncate screenHeight) (100, 100)


main :: IO ()
main = play
      window       -- A display
      white        -- Background color
      10           -- FPS
      initialGame  -- initial state
      drawGame     -- A function to convert the world a picture. SudokuGame -> Picture
      handleInput  -- A function to handle input events. Event -> SudokuGame -> SudokuGame
      (const id)   -- A function to step the world one iteration. Como não há mudança no tempo, permanece
