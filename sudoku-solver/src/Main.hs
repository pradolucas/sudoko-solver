import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.List (sortBy)

type Grid = [[Int]]

data SudokuGame = SudokuGame
  { grid :: Grid
  , selectedCell :: Maybe (Int, Int)
  , finished :: Bool
  }

initialGame :: SudokuGame
initialGame = SudokuGame initialGrid Nothing False

initialGrid :: Grid
initialGrid =
  [ [0, 3, 0, 0]
  , [0, 0, 2, 0]
  , [0, 0, 0, 1]
  , [0, 0, 0, 0]
  ]

window :: Display
window = InWindow "Sudoku 4x4" (400, 400) (100, 100)

drawGame :: SudokuGame -> Picture
drawGame game = pictures $
  [ translate (fromIntegral x * 100) (fromIntegral y * (-100)) $
    drawCell (grid game !! y !! x) (selectedCell game == Just (x, y))
  | x <- [0..3], y <- [0..3] ] ++ [drawFinishMessage game]

drawCell :: Int -> Bool -> Picture
drawCell val selected =
  pictures [ rectangleWire 100 100, drawValue val, highlightCell selected ]

drawValue :: Int -> Picture
drawValue val = translate (-30) (-30) $ scale 0.5 0.5 $ color black $
  text $ if val == 0 then "" else show val

highlightCell :: Bool -> Picture
highlightCell selected =
  if selected then color (makeColorI 0 0 255 100) $ rectangleSolid 100 100
              else blank

drawFinishMessage :: SudokuGame -> Picture
drawFinishMessage game =
  if finished game then translate (-100) 0 $ color green $ scale 0.5 0.5 $ text "Parabéns!"
                   else blank

handleInput :: Event -> SudokuGame -> SudokuGame
handleInput (EventKey (MouseButton LeftButton) Down _ mouse) game =
  case getClickedCell mouse of
    Just cell -> game { selectedCell = Just cell }
    Nothing   -> game { selectedCell = Nothing }
handleInput (EventKey (Char c) Down _ _) game@SudokuGame{ selectedCell = Just (x, y), grid = oldGrid }
  | c >= '1' && c <= '4' && isCellValid (x, y) (read [c]) oldGrid =
      game { grid = updateGrid (x, y) (read [c]) oldGrid, selectedCell = Nothing, finished = checkFinishedGrid (updateGrid (x, y) (read [c]) oldGrid) }
handleInput (EventKey (SpecialKey KeySpace) Down _ _) game@SudokuGame{ selectedCell = Just (x, y) } =
  game { grid = updateGrid (x, y) 0 (grid game), selectedCell = Nothing }
handleInput (EventKey (Char 'r') Down _ _) _ = initialGame
handleInput _ game = game

getClickedCell :: (Float, Float) -> Maybe (Int, Int)
getClickedCell (x, y)
  | x' < 0 || x' >= 4 || y' < 0 || y' >= 4 = Nothing
  | otherwise = Just (floor x', floor y')
  where
    x' = (x + 70) / 100
    y' = (y - 70) / (-100)

isCellValid :: (Int, Int) -> Int -> Grid -> Bool
isCellValid (x, y) val grid =
  all (/= val) (getRow y) && all (/= val) (getColumn x) && all (/= val) (getRegion x y)
  where
    getRow ry = grid !! ry
    getColumn rx = map (!! rx) grid
    getRegion rx ry =
      [ grid !! (y + ry') !! (x + rx')
      | rx' <- [0..1], ry' <- [0..1]
      ]
      where
        x = rx `div` 2 * 2
        y = ry `div` 2 * 2

updateGrid :: (Int, Int) -> Int -> Grid -> Grid
updateGrid (x, y) val grid =
  take y grid ++ [replaceAtIndex x val (grid !! y)] ++ drop (y + 1) grid

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n newVal xs = take n xs ++ [newVal] ++ drop (n + 1) xs

checkFinishedGrid :: Grid -> Bool
checkFinishedGrid = all (all (/= 0))

main :: IO ()
main = play window white 10 initialGame drawGame handleInput (const id)
