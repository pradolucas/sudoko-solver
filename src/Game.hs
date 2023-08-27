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
  , initialCells :: Grid
  , candidates :: [[(Int, [Int])]]
  , selectedCell :: Maybe (Int, Int)
  , finished :: Bool
  }
  --TODO initialCells :: Grid, celulas q n podem ser alteradas

-- Params

gridSize, blockSize :: Int
gridSize = 4
blockSize = truncate . sqrt $ fromIntegral gridSize

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
isFinished :: (Int, Int) -> Int -> Grid -> Bool
isFinished (x,y) val g =
  elementAppearsOnce val (getRow g x) -- checkLinha
  && elementAppearsOnce val (getColumn g y) -- checkColuna
  && elementAppearsOnce val (getRegion g x y) -- checkBloco
  where
    elementAppearsOnce element list = length (filter (== element) list) == 1

-- Somente válido durante a inserção
isCellValid :: (Int, Int) -> Int -> Grid -> Bool
isCellValid _ 0 _ = True
isCellValid (x, y) val g =
  notElem val (getRow g x) -- checkLinha
  && notElem val (getColumn g y) -- checkColuna
  && notElem val (getRegion g x y) -- checkBloco

getRow, getColumn:: Grid -> Int -> [Int]
getRow g x = g !! x
getColumn g y = map (!! y) g

getRegion :: Grid -> Int -> Int -> [Int]
getRegion g x y= [g !! i !! j | i<-[blockLowerBoundX x .. blockUpperBoundX x],
                     j<- [blockLowerBoundY y .. blockUpperBoundY y]]

blockLowerBoundX, blockUpperBoundX, blockLowerBoundY, blockUpperBoundY :: Int -> Int
blockLowerBoundX x = blockSize * (x `div` blockSize)
blockUpperBoundX x = blockLowerBoundX x + (blockSize-1)
blockLowerBoundY y = blockSize*(y `div` blockSize)
blockUpperBoundY y = blockLowerBoundY y + (blockSize-1)


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

-- Initialize candidates for each cell
initialCandidates :: Grid -> [[(Int, [Int])]]
initialCandidates g =  [
                    [(val, [val | val /= 0]) | val <- row ]
                    | row <- g
                    ]

isInitialValue :: (Int, Int) -> SudokuGame -> Bool
isInitialValue (i,j) g = (initialCells g !! i !!  j)  /= 0

initialGame :: SudokuGame
initialGame = SudokuGame
              initialGrid
              initialGrid
              (initialCandidates initialGrid)
              Nothing
              False


-- gridify :: Int -> [a] -> [[a]]
-- gridify _ [] = []
-- gridify n xs = take n xs : gridify n (drop n xs)

