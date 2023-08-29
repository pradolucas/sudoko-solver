module Render
   (drawGame
  ,drawCell
  ,drawValue
  ,highlightCell
  ,drawFinishMessage)
where

import Game ( SudokuGame(..), middleOfGridX, middleOfGridY, gridSize, cellWidth, cellHeight)
import Graphics.Gloss


drawGame :: SudokuGame -> Picture
drawGame game
  | menuActive game = drawMenu
  | otherwise = translate (-middleOfGridX) middleOfGridY frame
    where
      frame = pictures $
            [ translate (fromIntegral x * cellWidth) (fromIntegral y * (- cellHeight)) $
              drawCell (grid game !! x !! y) (selectedCell game == Just (x, y))
            | x <- [0..gridSize-1], y <- [0..gridSize-1] ] 
            ++ map (translate (cellWidth) (-cellHeight)) drawBlock
            ++ map (translate (4*cellWidth) (-cellHeight)) drawBlock
            ++ map (translate (7*cellWidth) (-cellHeight)) drawBlock

            ++ map (translate (cellWidth) (-4*cellHeight)) drawBlock
            ++ map (translate (4*cellWidth) (-4*cellHeight)) drawBlock
            ++ map (translate (7*cellWidth) (-4*cellHeight)) drawBlock

            ++ map (translate (cellWidth) (-7*cellHeight)) drawBlock
            ++ map (translate (4*cellWidth) (-7*cellHeight)) drawBlock
            ++ map (translate (7*cellWidth) (-7*cellHeight)) drawBlock
            ++ [drawFinishMessage game]

-- map (translate (cellWidth) (-cellHeight)) drawBlock
-- ++ map (translate (4*cellWidth) (-cellHeight)) drawBlock
-- ++ map (translate (4*cellWidth) (-cellHeight)) drawBlock

-- TODO ADD grid
thickRectangle :: Float -> Float -> [Picture]
thickRectangle width height =
  map (\w -> rectangleWire (width + w) (height + w)) [-2, -1, 0, 1, 2]
  -- <> [Color black $ rectangleSolid width height]

drawBlock :: [Picture]
drawBlock = thickRectangle width height
  where
    width  =  cellWidth * sqrt (fromIntegral gridSize)
    height =  cellHeight * sqrt (fromIntegral gridSize)

drawCell :: Int -> Bool -> Picture
drawCell val selected =
  pictures [ rectangleWire cellWidth cellHeight, drawValue val, highlightCell selected ]

drawValue :: Int -> Picture
drawValue val = translate (-15) (-15) $ scale (0.8 *(cellWidth/middleOfGridX)) (0.8 *(cellWidth/middleOfGridX)) $ color black $
  text $ if val == 0 then "" else show val

highlightCell :: Bool -> Picture
highlightCell selected =
  if selected then color (makeColorI 0 0 255 100) $ rectangleSolid cellWidth cellHeight
              else blank

drawFinishMessage :: SudokuGame -> Picture
drawFinishMessage game =
  if finished game
    then translate (-100) 0 $ color green $ scale 0.5 0.5 $ pictures
      [
       color blue $ rectangleSolid 8000 6000,
       translate (-90) (-10) $ color white $ text "Parabens, voce ganhou! :)"
      , color white $ translate (-150) (-100) $ scale 0.5 0.5 $ text "Aperte 'r' para jogar novamente"
      ]
    else blank




drawMenu :: Picture
drawMenu = translate (-100) 0 $ color green $ scale 0.5 0.5 $ pictures
      [ color blue $ rectangleSolid 8000 6000
      , translate (-300) (410) $ color white $ text "Sudoku"
      , color white $ translate (-300) (280) $ scale 0.5 0.5 $ text "Digite 'e' para easy"
      , color white $ translate (-300) (200) $ scale 0.5 0.5 $ text "Digite 'm' para medium"
      , color white $ translate (-300) (130) $ scale 0.5 0.5 $ text "Digite 'h' para hard"
      , color white $ translate (-300) (-100) $ scale 0.5 0.5 $ text "Regras do jogo:"
      , color white $ translate (-300) (-200) $ scale 0.5 0.5 $ text "Digite 'r' para reiniciar o jogo"
      , color white $ translate (-300) (-270) $ scale 0.5 0.5 $ text "Digite 'f' para mostrar a solucao"
      , color white $ translate (-300) (-340) $ scale 0.5 0.5 $ text "Para limpar a celula, selecione-a e aperte 'espaco'"
      , color white $ translate (-300) (-410) $ scale 0.5 0.5 $ text "Para dica: selecione uma celula e aperte 't'"
      ]
