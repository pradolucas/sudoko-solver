module Solver
where

import Game
    ( SudokuGame(..),
      Grid,
      gridSize,
      blockLowerBoundX,
      blockLowerBoundY, selectedCell, isCellValid )
import Data.List ((\\))
import Data.Maybe (mapMaybe)
import Logic (checkFinishedGrid)

-- -- Initialize candidates for each cell
-- initCandidates :: Grid -> [[(Int, [Int])]]
-- initCandidates g =  [
--                     [(val, [1..gridSize] \\ [val]) | val <- row ]
--                     | row <- g]

-- Update candidates after filling a cell
-- Add ao logic após ocorrer atualização
updateCandidates :: (Int, Int) -> Int -> [[(Int, [Int])]] -> [[(Int, [Int])]]
updateCandidates (x, y) newVal cands =
  [ [ if i == x && j == y then (val, [])  -- A cell que foi alterada, n tem mais candidatos
      else updateRelatedCells (i, j) (val, vals)
        | (j, (val, vals)) <- zip [0..] row ]
        | (i, row) <- zip [0..] cands
  ]
  where
    sameBlock (i1, j1) (i2, j2) = (blockLowerBoundX i1 == blockLowerBoundX i2) && (blockLowerBoundY j1 == blockLowerBoundY j2)
    updateRelatedCells (i, j) (v, vals)
      | i == x ||
        j == y ||
        sameBlock (i, j) (x, y) = (v, vals \\ [newVal])  -- Caso linha, coluna ou bloco do valor atualizado, apaga o valor como candidatos
      | otherwise = (v, vals)


-- gridFromCandidates :: [[(Int, [Int])]] -> Grid
-- gridFromCandidates = map (map fst)

gridCandidatesFromGame :: SudokuGame -> Grid
gridCandidatesFromGame g  = map (map fst) $ candidates g

-- constraintPropagate :: SudokuGame -> SudokuGame
-- constraintPropagate game =
--   let updatedCandidates = iterate (updatedCandidates (grid game)) (candidates game)
--       newGrid = gridFromCandidates (head updatedCandidates)
--    in game { grid = newGrid,
--              candidates = updatedCandidates,
--              finished = isFinishedGrid newGrid }


-- propagateCandidates :: SudokuGame -> [[(Int, [Int])]] -> [[(Int, [Int])]]
-- propagateCandidates game cands =
--   [ [ propagateCell (i, j) (val, vals)
--     | (j, (val, vals)) <- zip [0..] row ]
--     | (i, (row, candRow)) <- zip [0..] (zip game cands)
--   ]
--   where
--     propagateCell (i, j) (val, vals)
--       | val /= 0 = (val, [])
--       | otherwise = (0, eliminateCandidates (i, j) vals)

--     eliminateCandidates (i, j) vals =
--       foldr (\\) vals (relatedCellVals (i, j))

--     relatedCellVals (i, j) =
--       concatMap (cellCandidates $ candidates game) (relatedCells (i, j)) -- candidatos das celúlas relacionadas

--     relatedCells (i, j) = dropDuplicates $
--       rowCells i ++ colCells j ++ blockCells (i, j)   -- idx dos valores na mesma linha, coluna ou bloco, mais rápido que if. Repetindo a intersecção entre bloco e os demais. Testar

--     dropDuplicates = toList . fromList
--     rowCells i = [(i, col) | col <- [0..gridSize-1]]
--     colCells j = [(row, j) | row <- [0..gridSize-1]]
--     blockCells (i, j) = [(r, c) | r <- [blockLowerBoundX  i .. blockUpperBoundX i], c <- [blockLowerBoundY j .. blockUpperBoundY j]]

-- cellCandidates :: [[(Int, [Int])]] -> (Int, Int) -> [Int]
-- cellCandidates g (i,j) = snd (g !! i !! j)

-- Solve the Sudoku puzzle using constraint propagation
constraintSudokuSolver :: SudokuGame -> Maybe Grid
constraintSudokuSolver game
  | finished game = Just (grid game)
  | otherwise =
    case solverSelectedCell game of
      Just (row, col) ->
        let validValues = snd $ candidates game !! row !! col
            nextGames = map (\val -> fillCellWithCandidate (row, col) val game) validValues
            solutions = mapMaybe constraintSudokuSolver nextGames -- Chamada recursiva com todas possibilidades de preenchimento para celula vigente
        in findSolution solutions
      Nothing -> Nothing
  where
    findSolution [] = Nothing
    findSolution (sol : _) = Just sol --  Pegue a primeira solução que aparecer

selectNextCell :: (Int, Int) -> Maybe (Int, Int)
selectNextCell (row, col)
  | col + 1 < gridSize = Just (row, col + 1)  -- Move to the next column in the same row
  | row + 1 < gridSize = Just (row + 1, 0)     -- Move to the first column of the next row
  | otherwise = Nothing                         -- No more cells left to fill

fillCellWithCandidate :: (Int, Int) -> Int -> SudokuGame -> SudokuGame
fillCellWithCandidate (i, j) val game
  | isCellValid (i, j) val gridCandidates =
    let updatedCandidates = updateCandidates (i, j) val (candidates game)
        newSelectedCell = selectNextCell (i, j)
      in game { candidates = updatedCandidates,
                selectedCell = newSelectedCell,
                finished = checkFinishedGrid gridCandidates }
  | otherwise =  game

  where
    gridCandidates = gridCandidatesFromGame game


-- main :: IO ()
-- main = do
--   let solution = constraintSudokuSolver initialGame
--   case solution of
--     Just sol -> printGrid sol
--     Nothing -> putStrLn "No solution found."

-- printGrid :: Grid -> IO ()
-- printGrid g = mapM_ (putStrLn . unwords . map show) g