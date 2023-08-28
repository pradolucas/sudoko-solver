import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.List (sortBy)

type Grid = [[Int]]

data SudokuGame = SudokuGame
  { grid :: Grid
  , selectedCell :: Maybe (Int, Int)
  , finished :: Bool
  , menuActive :: Bool
  }

initialGame :: SudokuGame
initialGame = SudokuGame initialGrid Nothing False True

initialGrid :: Grid
initialGrid = replicate 9 (replicate 9 0)

drawMenu :: Picture
drawMenu = translate (-100) 0 $ color green $ scale 0.5 0.5 $ pictures
      [ color blue $ rectangleSolid 8000 6000
      , translate (-90) (-10) $ color white $ text "Sudoku"
      , color white $ translate (-150) (-100) $ scale 0.5 0.5 $ text "Digite 'f' para facil"
      , color white $ translate (-150) (-180) $ scale 0.5 0.5 $ text "Digite 'm' para medio"
      , color white $ translate (-150) (-250) $ scale 0.5 0.5 $ text "Digite 'd' para dificil"
      ]

window :: Display
window = InWindow "Sudoku 9x9" (600, 600) (100, 100)

drawGame :: SudokuGame -> Picture
drawGame game 
  | menuActive game = drawMenu
  | otherwise = pictures $
      [ translate (fromIntegral x * 60) (fromIntegral y * (-60)) $
      drawCell (grid game !! y !! x) (selectedCell game == Just (x, y))
      | x <- [0..8], y <- [0..8] ] ++ [drawFinishMessage game]


drawCell :: Int -> Bool -> Picture
drawCell val selected =
  pictures [ rectangleWire 60 60, drawValue val, highlightCell selected ]

drawValue :: Int -> Picture
drawValue val = translate (-15) (-15) $ scale 0.3 0.3 $ color black $
  text $ if val == 0 then "" else show val

highlightCell :: Bool -> Picture
highlightCell selected =
  if selected then color (makeColorI 0 0 255 100) $ rectangleSolid 60 60
              else blank

drawFinishMessage :: SudokuGame -> Picture
drawFinishMessage game =
  if finished game then translate (-100) 0 $ color green $ scale 0.5 0.5 $ text "Parabéns!"
                   else blank

getClickedCell :: (Float, Float) -> Maybe (Int, Int)
getClickedCell (x, y)
  | x' < 0 || x' >= 9 || y' < 0 || y' >= 9 = Nothing
  | otherwise = Just (floor x', floor y')
  where
    x' = (x + 270) / 60
    y' = (y - 270) / (-60)

isCellValid :: (Int, Int) -> Int -> Grid -> Bool
isCellValid (x, y) val grid =
  all (/= val) (getRow y) && all (/= val) (getColumn x) && all (/= val) (getRegion x y)
  where
    getRow ry = grid !! ry
    getColumn rx = map (!! rx) grid
    getRegion rx ry =
      [ grid !! (y + ry') !! (x + rx')
      | rx' <- [0..2], ry' <- [0..2]
      ]
      where
        x = rx `div` 3 * 3
        y = ry `div` 3 * 3

updateGrid :: (Int, Int) -> Int -> Grid -> Grid
updateGrid (x, y) val grid =
  take y grid ++ [replaceAtIndex x val (grid !! y)] ++ drop (y + 1) grid

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n newVal xs = take n xs ++ [newVal] ++ drop (n + 1) xs

checkFinishedGrid :: Grid -> Bool
checkFinishedGrid = all (all (/= 0))

handleInput :: Event -> SudokuGame -> SudokuGame
handleInput (EventKey (Char key) Down _ _) game@SudokuGame{ menuActive = True }
  | key == 'f' || key == 'm' || key == 'd' = game { menuActive = False }
handleInput e game
  | menuActive game = game
  | otherwise = case e of
    EventKey (MouseButton LeftButton) Down _ mouse ->
      case getClickedCell mouse of
        Just cell -> game { selectedCell = Just cell }
        Nothing   -> game { selectedCell = Nothing }
    EventKey (Char c) Down _ _ ->
      case selectedCell game of
        Just (x, y) | c >= '1' && c <= '9' && isCellValid (x, y) (read [c]) (grid game) ->
          game { grid = updateGrid (x, y) (read [c]) (grid game)
               , selectedCell = Nothing
               , finished = checkFinishedGrid (updateGrid (x, y) (read [c]) (grid game))
               }
        _ -> game
    EventKey (SpecialKey KeySpace) Down _ _ ->
      case selectedCell game of
        Just (x, y) -> game { grid = updateGrid (x, y) 0 (grid game), selectedCell = Nothing }
        _ -> game
    EventKey (Char 'r') Down _ _ -> initialGame
    _ -> game

main :: IO ()
main = play window white 10 initialGame drawGame handleInput (const id)