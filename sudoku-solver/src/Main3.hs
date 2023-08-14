import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

type Grid = [[Int]]

data SudokuGame = Game
  { grid :: Grid
  , selectedCell :: (Int, Int)
  }

initialGame :: SudokuGame
initialGame = Game initialGrid (0, 0)

initialGrid :: Grid
initialGrid = replicate 4 $ replicate 4 0

window :: Display
window = InWindow "Sudoku 4x4" (400, 400) (100, 100)

drawGame :: SudokuGame -> Picture
drawGame (Game grid (selectedRow, selectedCol)) = pictures
  [ drawGrid grid
  , drawSelection selectedRow selectedCol
  ]

drawGrid :: Grid -> Picture
drawGrid grid = pictures $
  [ translate (fromIntegral x * 100) (fromIntegral y * (-100)) $
    drawCell val | (x, row) <- zip [0..] grid, (y, val) <- zip [0..] row ]

drawCell :: Int -> Picture
drawCell val = pictures
  [ rectangleWire 100 100
  , translate (-30) (-30) $ scale 0.5 0.5 $ color black $ text (show val)
  ]

drawSelection :: Int -> Int -> Picture
drawSelection row col = translate (fromIntegral col * 100) (fromIntegral row * (-100)) $
  color (makeColor 0 0 1 0.3) $ rectangleSolid 100 100

handleInput :: Event -> SudokuGame -> SudokuGame
handleInput (EventKey (MouseButton LeftButton) Down _ mousePos) game =
  let (row, col) = mouseToCell mousePos
   in game { selectedCell = (row, col) }
handleInput (EventKey (Char c) Down _ _) game@(Game grid (row, col))
  | c >= '1' && c <= '4' =
      let updatedGrid = updateGrid row col (read [c]) grid
       in game { grid = updatedGrid }
handleInput _ game = game

mouseToCell :: Point -> (Int, Int)
mouseToCell (x, y) = (floor $ y / (-100), floor $ x / 100)

updateGrid :: Int -> Int -> Int -> Grid -> Grid
updateGrid row col val grid = replaceAtIndex row newRow grid
  where
    newRow = replaceAtIndex col val (grid !! row)

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n newVal xs = take n xs ++ [newVal] ++ drop (n + 1) xs

main :: IO ()
main = play window white 10 initialGame drawGame handleInput (const id)
