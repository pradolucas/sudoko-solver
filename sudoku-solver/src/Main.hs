
module Main (main) where
import Graphics.Gloss

-- Função para desenhar um quadrado com linhas
drawSquareWithLines :: Picture
drawSquareWithLines = pictures [square, horizontalLines, verticalLines]
  where
    square = color white $ rectangleSolid 400 400
    horizontalLines = pictures [color black $ rectangleSolid 400 2 | y <- [-198, -196 .. 198]]
    verticalLines = pictures [color black $ rectangleSolid 2 400 | x <- [-198, -196 .. 198]]

-- Função principal para exibir a imagem
main :: IO ()
main = display (InWindow "Quadrado com Linhas" (400, 400) (10, 10)) white drawSquareWithLines
