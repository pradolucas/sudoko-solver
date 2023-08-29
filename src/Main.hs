module Main (main) where

-- import Game
import Game ( initialGame, screenWidth, screenHeight, gridSize)
import Render ( drawGame )
import Logic ( handleInput )

import Graphics.Gloss ( white, play, Display(InWindow) )
import Data.Char (digitToInt)


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


-- main :: IO ()
-- main = do
--       let ss = [".56.3.87...928.65.2..6.....5.31..2..6.2..35...9.8.2..68..3..42.3...48...94....1..", -- 3 
--             "..3.78.2....1.97.4.7.5628...32.....15..62.43..4.38.25.6...1.9.231.29.87....5.31..",  -- 5
--             "...589.4..9.41....415.2...958.7..4.3...8..69.7....2815.6.145..8..7.68..4...97.162",  -- 7
--             "..7.648916..98.4.784.21763..8..........6..5.2.....91.47.8.31...1.....356..2.....8",  -- 9
--             ".546218..281.5.6.46..84..2..1.2...59.4...67.....41..36.....2..1.387...6242..6...7"  -- 10
--             ]
--       print $ map parseSudoku ss
       

-- parseSudoku :: String -> [[Int]]
-- parseSudoku input = gridFy gridS $ map charToNum input
--   where
--     charToNum '.' = 0
--     charToNum c = digitToInt c
--     gridS = truncate . sqrt $ fromIntegral $ length input

-- gridFy :: Int -> [a] -> [[a]]
-- gridFy _ [] = []
-- gridFy n xs = take n xs : gridFy n (drop n xs)