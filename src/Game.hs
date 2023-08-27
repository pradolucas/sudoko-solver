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
    ( fromList, evalRand, mkStdGen, RandomGen )
import Data.Digest.Murmur32 ( asWord32, hash32 ) 

type Grid = [[Int]]

data SudokuGame = SudokuGame
  { grid :: Grid
  , selectedCell :: Maybe (Int, Int)
  , finished :: Bool
  }

-- Params

gridSize :: Int
gridSize = 4

screenWidth, screenHeight :: Float
screenWidth = 400
screenHeight = 400

cellWidth, cellHeight :: Float
cellWidth = screenWidth / fromIntegral gridSize
cellHeight = screenHeight / fromIntegral gridSize

middleOfGridX, middleOfGridY :: Float
middleOfGridX = screenWidth * 0.5
middleOfGridY = screenHeight * 0.5


-- Randomness

weightedDistribution :: [(Int, Rational)]
weightedDistribution = (0, 5) : [(i, 1) | i<-[1..(gridSize-1)]]
-- a dificuldade vai será determinada pela probabilidade de 0

customDistribution :: RandomGen g => g -> [(a, Rational)] -> [a]
customDistribution gen weights = evalRand m gen
    where m = sequence . repeat . fromList $ weights

hashSeed :: (Num b, Show a1, Show a2) => a1 -> a2 -> b
hashSeed row col = fromIntegral $ asWord32 $ hash32 (show row ++ "," ++ show col)

-- Validate 

isGridValid :: Grid -> [Bool]
isGridValid g = [isFinished (i,j) (g !! i !! j) g
                | i<-[0..(gridSize-1)], j<-[0..(gridSize-1)]]

-- Double check, apagar posteriormente
isFinished :: Eq a => (Int, Int) -> a -> [[a]] -> Bool
isFinished (x,y) val gr = 
  elementAppearsOnce val getRow -- checkLinha
  && elementAppearsOnce val getColumn -- checkColuna
  && elementAppearsOnce val getRegion -- checkBloco
  where
    elementAppearsOnce element list = length (filter (== element) list) == 1
    getRow = gr !! x
    getColumn = map (!! y) gr
    getRegion = [gr !! i !! j | i<-[blockLowerBoundX .. blockUpperBoundX],
                     j<- [blockLowerBoundY .. blockUpperBoundY]]
    blockSize = truncate . sqrt $ fromIntegral gridSize
    blockLowerBoundX = blockSize * (x `div` blockSize)
    blockUpperBoundX = blockLowerBoundX + (blockSize-1)
    blockLowerBoundY = blockSize*(y `div` blockSize)
    blockUpperBoundY = blockLowerBoundY + (blockSize-1)

-- Somente válido durante a inserção
isCellValid :: (Int, Int) -> Int -> Grid -> Bool
isCellValid _ 0 _ = True
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


-- Update State

updateGrid :: (Int, Int) -> Int -> Grid -> Grid
updateGrid (x, y) val g =
  take x g ++ [replaceAtIndex y val (g !! x)] ++ drop (x + 1) g

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n newVal xs = take n xs ++ [newVal] ++ drop (n + 1) xs

fillCellRandomValue :: (Int, Int) -> Int -> Grid -> Grid
fillCellRandomValue (i,j) seed g
  | isCellValid (i,j) val g = updateGrid (i,j) val g
  | otherwise               = fillCellRandomValue (i,j) (hashSeed seed 1) g
  where
    val = head $ take 1 $ customDistribution (mkStdGen seed) weightedDistribution


runFillGrid :: Grid -> Grid
runFillGrid g = foldl (\acc (i, j) -> fillCellRandomValue (i,j) (hashSeed i j) acc ) g elementOrder
  where
    elementOrder = [(r, c) | c <- [0 .. gridSize - 1], r <- [0 .. gridSize - 1]]

-- Garantir que tem solução, senão chamar novamente
initialGrid :: Grid
initialGrid = runFillGrid $ replicate gridSize (replicate gridSize 0)


initialGame :: SudokuGame
initialGame = SudokuGame initialGrid Nothing False



-- gridify :: Int -> [a] -> [[a]]
-- gridify _ [] = []
-- gridify n xs = take n xs : gridify n (drop n xs)

