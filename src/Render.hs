module Render 
   (drawGame
  ,drawCell
  ,drawValue
  ,highlightCell
  ,drawFinishMessage) 
where

import Game ( SudokuGame(..), middleOfGridX, middleOfGridY, gridSize, cellWidth, cellHeight)
import Graphics.Gloss
    ( black,
      green,
      blank,
      color,
      pictures,
      rectangleSolid,
      rectangleWire,
      scale,
      text,
      translate,
      makeColorI,
      Picture )

drawGame :: SudokuGame -> Picture
drawGame game = translate (-middleOfGridX)                          -- puts in the center
                          middleOfGridY
                          frame   
    where
    frame = pictures $
            [ translate (fromIntegral x * cellWidth) (fromIntegral y * (- cellHeight)) $
              drawCell (grid game !! x !! y) (selectedCell game == Just (x, y))
            | x <- [0..gridSize-1], y <- [0..gridSize-1] ] ++ [drawFinishMessage game]

drawCell :: Int -> Bool -> Picture
drawCell val selected =
  pictures [ rectangleWire cellWidth cellHeight, drawValue val, highlightCell selected ]



drawValue :: Int -> Picture
drawValue val = translate (-15) (-15) $ scale (1.2 *(cellWidth/middleOfGridX)) (1.2 *(cellWidth/middleOfGridX)) $ color black $
  text $ if val == 0 then "" else show val
-- TODO Ajustar escala e translate a se adequar ao grid
highlightCell :: Bool -> Picture
highlightCell selected =
  if selected then color (makeColorI 0 0 255 100) $ rectangleSolid cellWidth cellHeight
              else blank

-- TODO deixar mais bonito, 
drawFinishMessage :: SudokuGame -> Picture
drawFinishMessage game =
  if finished game 
    then translate (-100) 0 $ color green $ scale 0.5 0.5 $ text "Parabéns!" 
    else blank