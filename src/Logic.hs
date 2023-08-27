module Logic where

import Game ( SudokuGame(..), Grid, initialGame, middleOfGridX, cellWidth, middleOfGridY, cellHeight, gridSize )
import Graphics.Gloss.Interface.IO.Game
    ( Key(Char, MouseButton, SpecialKey),
      Event(EventKey),
      KeyState(Down),
      MouseButton(LeftButton),
      SpecialKey(KeySpace) )


handleInput :: Event -> SudokuGame -> SudokuGame
handleInput (EventKey (MouseButton LeftButton) Down _ mouse) game = case getClickedCell mouse of
                                                                    Just cell -> game { selectedCell = Just cell }
                                                                    Nothing   -> game { selectedCell = Nothing }

-- Prenchimento da celula
handleInput (EventKey (Char c) Down _ _) game@SudokuGame{ selectedCell = Just (x, y), grid = oldGrid }
  | c >= '1' && c <= '9' && isCellValid (x, y) (read [c]) oldGrid =
      game { grid = updateGrid (x, y) (read [c]) oldGrid, selectedCell = Nothing, finished = checkFinishedGrid (updateGrid (x, y) (read [c]) oldGrid) }

-- Apagar valor anterior, só toma caso tenha uma selectedCell = Just
handleInput (EventKey (SpecialKey KeySpace) Down _ _) game@SudokuGame{ selectedCell = Just (x, y) } =
  game { grid = updateGrid (x, y) 0 (grid game), selectedCell = Nothing }

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
-- tornar esse valor 50 baseado no gridSize
isCellValid :: (Int, Int) -> Int -> Grid -> Bool
isCellValid (x, y) val g =
  notElem val getRow -- checkLinha
  && notElem val getColumn -- checkColuna
  && notElem val getRegion -- checkBloco
  where
    getRow = g !! x
    getColumn = map (!! y) g
    getRegion =
      [g !! i !! j | i<-[blockLowerBoundX .. blockUpperBoundX],
                     j<- [blockLowerBoundY .. blockUpperBoundY]]
      where
        blockSize = truncate . sqrt $ fromIntegral gridSize
        blockLowerBoundX = blockSize * (x `div` blockSize)
        blockUpperBoundX = blockLowerBoundX + (blockSize-1)
        blockLowerBoundY = blockSize*(y `div` blockSize)
        blockUpperBoundY = blockLowerBoundY + (blockSize-1)


updateGrid :: (Int, Int) -> Int -> Grid -> Grid
updateGrid (x, y) val g =
  take x g ++ [replaceAtIndex y val (g !! x)] ++ drop (x + 1) g

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n newVal xs = take n xs ++ [newVal] ++ drop (n + 1) xs

checkFinishedGrid :: Grid -> Bool
checkFinishedGrid = all (notElem 0)
