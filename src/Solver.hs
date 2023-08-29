module Solver
  (constraintSudokuSolver)
where

import Game
import Data.Maybe (mapMaybe)

gridCandidatesFromGame :: SudokuGame -> Grid
gridCandidatesFromGame g  = map (map fst) $ candidates g

-- Solve the Sudoku puzzle using constraint propagation
constraintSudokuSolver :: SudokuGame -> Maybe Grid
constraintSudokuSolver game
  | finished game = Just (gridCandidatesFromGame game)
  | otherwise =
    case solverSelectedCell game of
      Just (row, col) ->
        let currentCell = candidates game !! row !! col
            validCandidatesValues = snd currentCell 
            nextGames 
              | null validCandidatesValues && fst currentCell == 0      = []
              | null validCandidatesValues  = [game {solverSelectedCell = selectNextCell (row, col)}] -- Valor correto, próximo
              | otherwise = mapMaybe (\val -> fillCellWithCandidate (row, col) val game) validCandidatesValues
            solutions = mapMaybe constraintSudokuSolver nextGames -- Chamada recursiva com todas possibilidades de preenchimento para celula vigente
        in findSolution solutions
      Nothing -> Nothing
  where
    findSolution [] = Nothing
    findSolution (sol : _) = Just sol --  Pegue a primeira solução que aparecer

fillCellWithCandidate :: (Int, Int) -> Int -> SudokuGame -> Maybe SudokuGame
fillCellWithCandidate (i, j) val game
  | isCellValid (i, j) val gridCandidates =
    let updatedCandidates = Game.updateCandidates (i, j) val (candidates game)
        newSelectedCell = selectNextCell (i, j)
        newGrid = game { candidates = updatedCandidates,
                solverSelectedCell = newSelectedCell}
      in Just $ newGrid{ finished = checkFinishedGrid $ gridCandidatesFromGame newGrid }
  | otherwise =  Nothing 
    where
      gridCandidates = gridCandidatesFromGame game