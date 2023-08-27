module Game
  -- (SudokuGame (..),
  -- Grid,
  -- gridSize,
  -- screenWidth,
  -- screenHeight,
  -- cellWidth,
  -- cellHeight,
  -- middleOfGridX,
  -- middleOfGridY,
  -- initialGrid,
  -- initialGame
  -- )
   where
import Control.Monad.Random


type Grid = [[Int]]

data SudokuGame = SudokuGame
  { grid :: Grid
  , selectedCell :: Maybe (Int, Int)
  , finished :: Bool
  }


gridSize :: Int
gridSize = 9

screenWidth, screenHeight :: Float
screenWidth = 400
screenHeight = 400

cellWidth, cellHeight :: Float
cellWidth = screenWidth / fromIntegral gridSize
cellHeight = screenHeight / fromIntegral gridSize

middleOfGridX, middleOfGridY :: Float
middleOfGridX = screenWidth * 0.5
middleOfGridY = screenHeight * 0.5

weightedValues :: [(Int, Rational)]
weightedValues = (0, 10) : [(i, 1) | i<-[1..(gridSize-1)]] 
-- a dificuldade vai será determinada pela probabilidade de 0

weightedList :: RandomGen g => g -> [(a, Rational)] -> [a]
weightedList gen weights = evalRand m gen
    where m = sequence . repeat . fromList $ weights
-- run validation of the initial 

gridify :: Int -> [a] -> [[a]]
gridify _ [] = []
gridify n xs = take n xs : gridify n (drop n xs)

initialGrid :: Grid
initialGrid = gridify gridSize $
              take (gridSize*gridSize) $
              weightedList (mkStdGen 2) weightedValues
-- initialGrid = replicate gridSize (replicate gridSize 0)
-- initialGrid =  [ [0, 3, 0, 0]
--   , [0, 0, 2, 0]
--   , [0, 0, 0, 1]
--   , [0, 0, 0, 0]
--   ]


initialGame :: SudokuGame
initialGame = SudokuGame initialGrid Nothing False

