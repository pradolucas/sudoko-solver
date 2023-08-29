module Game
  (SudokuGame (..),
  Grid,
  gridSize,
  screenWidth,
  screenHeight,
  cellWidth,
  cellHeight,
  middleOfGridX,
  middleOfGridY,
  initialGrid,
  initialGame,
  updateCandidates,
  updateGrid,
  candidatesFromGrid,
  isCellValid,
  isInitialValue,
  selectNextCell,
  checkFinishedGrid,
  )
  where
import Control.Monad.Random
    ( fromList,
      evalRand,
      getStdRandom,
      mkStdGen,
      Random(randomR),
      RandomGen )

import Data.Digest.Murmur32 ( asWord32, hash32 )
import Data.List ((\\), intercalate)
import System.Random ()
import System.IO.Unsafe ( unsafePerformIO )
import Data.Set (toList, fromList)

type Grid = [[Int]]

data SudokuGame = SudokuGame
  { grid :: Grid
  , initialCells :: Grid
  , candidates :: [[(Int, [Int])]]
  , solverSelectedCell :: Maybe (Int, Int)
  , selectedCell :: Maybe (Int, Int)
  , finished :: Bool
  , menuActive :: Bool
  }
  -- deriving (Show)

instance Show SudokuGame where
  show g = gridShow ++ separator ++ candidatesShow
    where
      gridShow = "Grid:\n" ++ unlines (map (intercalate " | " . map show) (grid g))
      candidatesShow = "Candidates:\n" ++ unlines (map (intercalate " | " . map show) (candidates g))
      separator = replicate 20 '_' ++ "\n\n"


-- Params

gridSize, blockSize :: Int
gridSize = 9
blockSize = truncate . sqrt $ fromIntegral gridSize -- Quadrante

screenWidth, screenHeight :: Float
screenWidth = 800
screenHeight = 600

cellWidth, cellHeight :: Float
cellWidth = screenWidth / fromIntegral gridSize
cellHeight = screenHeight / fromIntegral gridSize

middleOfGridX, middleOfGridY :: Float
middleOfGridX = screenWidth * 0.5
middleOfGridY = screenHeight * 0.5

generateRandomNumber :: Int
{-# NOINLINE generateRandomNumber #-}
generateRandomNumber = unsafePerformIO (getStdRandom (randomR (1, 3)))


-- Randomness

weightedDistribution :: [(Int, Rational)]
weightedDistribution = (0, 5) : [(i, 1) | i<-[1..(gridSize-1)]]
-- a dificuldade vai será determinada pela probabilidade de 0

customDistribution :: RandomGen g => g -> [(a, Rational)] -> [a]
customDistribution gen weights = evalRand m gen
    where m = sequence . repeat . Control.Monad.Random.fromList $ weights

hashSeed :: (Num b, Show a1, Show a2) => a1 -> a2 -> b
hashSeed row col =  12 + fromIntegral (asWord32 $ hash32 (show row ++ "," ++ show col))


-- Validate 

isGridValid :: Grid -> [Bool]
isGridValid g = [isFinished (i,j) (g !! i !! j) g && (g !! i !! j) /=0
                | i<-[0..(gridSize-1)], j<-[0..(gridSize-1)]]

isFinished :: (Int, Int) -> Int -> Grid -> Bool
isFinished (x,y) val g =
  elementAppearsOnce val (getRow g x) -- checkLinha
  && elementAppearsOnce val (getColumn g y) -- checkColuna
  && elementAppearsOnce val (getRegion g x y) -- checkBloco
  where
    elementAppearsOnce element list = length (filter (== element) list) == 1

checkFinishedGrid :: Grid -> Bool
checkFinishedGrid g = and (isGridValid g)

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

updateCandidates :: (Int, Int) -> Int -> [[(Int, [Int])]] -> [[(Int, [Int])]]
updateCandidates (x, y) newVal cands
  | newVal == 0 = resetCandidates (x,y) cands
  | otherwise    = updatedCandidates

    where
      
      updateRelatedCells (i, j) (v, vals)
        -- | i == x && j == y && newVal == 0   = (newVal, [1..gridSize] \\ valuesFromRelated (i,j) cands) -- A cell que foi alterada, n tem mais candidatos
        | i == x && j == y = (newVal, []) -- A cell que foi alterada, n tem mais candidatos
        | i == x || j == y || sameBlock (i, j) (x, y) = (v, vals \\ [newVal])  -- Caso linha, coluna ou bloco do valor atualizado, apaga o valor como candidatos
        | otherwise = (v, vals)

      updatedCandidates = [[updateRelatedCells (i, j) (val, vals)
          | (j, (val, vals)) <- zip [0..] row ]
          | (i, row) <- zip [0..] cands]

resetCandidates :: (Int, Int) -> [[(Int, [Int])]] -> [[(Int, [Int])]]
resetCandidates (x, y) cands =
    [[  update0 (i, j) (val, vals)
        | (j, (val, vals)) <- zip [0..] row ]
        | (i, row) <- zip [0..] cands]
    where
      oldValue = fst $ cands !! x !! y
      possibleValue 
        | oldValue == 0 = []
        | otherwise     = [oldValue]
      
      update0 (i, j) (val, vals)
        | x==i && y==j  = (0, [1..gridSize] \\ valuesFromRelated (i,j) cands)
        | i == x || j == y || sameBlock (i, j) (x, y) = 
            case vals of
                  [] -> (val, [])  -- caso já esteja preenchido, n atualizar candidatos
                  xs -> (val, xs ++ possibleValue)
        | otherwise = (val, vals)

sameBlock :: (Int, Int) -> (Int, Int) -> Bool
sameBlock (i1, j1) (i2, j2) = (blockLowerBoundX i1 == blockLowerBoundX i2) && (blockLowerBoundY j1 == blockLowerBoundY j2)

dropDuplicates :: Ord a => [a] -> [a]
dropDuplicates x = toList ( Data.Set.fromList x)

valuesFromRelated :: (Int, Int) -> [[(Int, [Int])]] -> [Int]
valuesFromRelated (i, j) g = dropDuplicates $ map (\(x,y) -> fst $ g !! x !! y) relatedIdx
    where
      relatedIdx = rowCells i ++ colCells j ++ blockCells (i, j)
      rowCells x = [(x, col) | col <- [0..gridSize-1]]
      colCells y = [(row, y) | row <- [0..gridSize-1]]
      blockCells (x, y) = [(r, c) | r <- [blockLowerBoundX  x .. blockUpperBoundX x], c <- [blockLowerBoundY y .. blockUpperBoundY y]]

selectNextCell :: (Int, Int) -> Maybe (Int, Int)
selectNextCell (row, col)
  | col + 1 < gridSize = Just (row, col + 1)  -- Move to the next column in the same row
  | row + 1 < gridSize = Just (row + 1, 0)     -- Move to the first column of the next row
  | otherwise = Nothing                         -- No more cells left to fill


-- Initialization

fillCellRandomValue :: (Int, Int) -> Int -> Grid -> Grid
fillCellRandomValue (i,j) seed g
  | isCellValid (i,j) val g = updateGrid (i,j) val g
  | otherwise               = fillCellRandomValue (i,j) (hashSeed seed seed+1) g
  where
    val = head $ take 1 $ customDistribution (mkStdGen seed) weightedDistribution

runFillGrid :: Grid -> Grid
runFillGrid g = foldl (\acc (i, j) -> fillCellRandomValue (i,j) (hashSeed i j) acc ) g elementOrder
  where
    elementOrder = [(r, c) | c <- [0 .. gridSize - 1], r <- [0 .. gridSize - 1]]

-- Garantir que tem solução, senão chamar novamente
initialGrid :: Grid
-- initialGrid = runFillGrid $ replicate gridSize (replicate gridSize 0)
-- facil
-- initialGrid = [[0,0,3,0,7,8,0,2,0],[0,0,0,1,0,9,7,0,4],[0,7,0,5,6,2,8,0,0],[0,3,2,0,0,0,0,0,1],[5,0,0,6,2,0,4,3,0],[0,4,0,3,8,0,2,5,0],[6,0,0,0,1,0,9,0,2],[3,1,0,2,9,0,8,7,0],[0,0,0,5,0,3,1,0,0]]
-- dificil
initialGrid = [[0,0,0,4,0,0,0,0,0],[7,0,0,9,0,0,0,0,0],[0,0,0,0,0,0,7,0,3],[9,0,0,0,0,0,0,0,5],[0,0,4,0,9,0,8,0,1],[0,0,0,0,0,0,0,0,0],[1,0,0,2,5,0,0,8,0],[3,4,6,0,1,8,0,0,9],[2,0,0,0,0,9,0,0,6]]
-- initialGrid =  [ [0, 0, 0, 0]
--                 , [3, 0, 0, 0]
--                 , [0, 2, 0, 0]
--                 , [0, 0, 1, 0]
--               ]

initialCand :: [[(Int, [Int])]]
initialCand = replicate gridSize (replicate gridSize (0, [1..gridSize]))

-- Initialize candidates for each cell

candidatesFromGrid :: [[(Int, [Int])]] -> Grid -> [[(Int, [Int])]]
candidatesFromGrid cand g = auxCandidatesFromGrid cand (Just (0,0)) g

auxCandidatesFromGrid :: [[(Int, [Int])]] -> Maybe (Int, Int) -> Grid -> [[(Int, [Int])]]
auxCandidatesFromGrid cand Nothing _ = cand
auxCandidatesFromGrid cand (Just (i,j)) g = auxCandidatesFromGrid acc (selectNextCell (i, j)) g
    where
      acc = updateCandidates (i,j) (g !! i !! j) cand

-- auxCandidatesFromGrid :: Grid -> [[(Int, [Int])]]
-- auxCandidatesFromGrid g =  foldl (\acc (i,j, val) -> updateCandidates (i,j) val acc) initialCand elements
--                   where
--                     elements = [(i,j,val) | (i, row) <- zip [0..] g, (j, val) <- zip [0..] row]  


isInitialValue :: (Int, Int) -> SudokuGame -> Bool
isInitialValue (i,j) g = (initialCells g !! i !!  j)  /= 0

initialGame :: SudokuGame
initialGame = SudokuGame
              initialGrid -- grid
              initialGrid -- initialCells
              (candidatesFromGrid initialCand initialGrid) -- candidates
              (Just (0,0)) -- solverSelectedCell
              -- Nothing      -- solvedGrid
              Nothing      -- selectCell
              False        -- finished
              True        -- menuActive





