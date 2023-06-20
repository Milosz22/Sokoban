module Render where
import Graphics.Gloss
import Game


tileSize :: Float
tileSize = 50 

translateX :: Float 
translateX = -200
translateY :: Float 
translateY = -200

render :: World -> Picture
render world = pictures $ mapPic ++ playerPic ++ boxesPic 
  where
    mapPic = concatMap drawRow (zip [0..] ( gameMap world))
    drawRow (y, row) = map (drawTile y) (zip [0..] row)
    drawTile y (x, tile) = translate ((translateX) + (tileSize * fromIntegral x )) ((translateY) +tileSize * fromIntegral y) (drawSpecificTile tile)

    drawSpecificTile tile = case tile of
      Wall -> color black $ rectangleSolid tileSize tileSize
      Ground -> color white $ rectangleSolid tileSize tileSize
      Empty -> blank
      Exit -> color yellow $ rectangleSolid tileSize tileSize

    playerPic = [translate (translateX + (tileSize * fromIntegral x)) (translateY + (tileSize * fromIntegral y)) $ color blue $ rectangleSolid tileSize tileSize | (x, y) <- [player world]]

    boxesPic = [translate (translateX + (tileSize * fromIntegral x)) (translateY + (tileSize * fromIntegral y)) $ color red $ rectangleSolid tileSize tileSize | (x, y) <- boxes world]


