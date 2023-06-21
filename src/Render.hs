{-# LANGUAGE RecordWildCards #-}
module Render where
import Graphics.Gloss
import Game
import qualified Data.Sequence as S
import Data.Sequence ((><), (<|), (|>), ViewL(..), ViewR(..), viewl, viewr)
import Data.Foldable (toList)


tileSize :: Float
tileSize = 50 

translateX :: Float 
translateX = -200
translateY :: Float 
translateY = -200

successTextX :: Float 
successTextX = -150
successTextY :: Float 
successTextY =  150




render :: World -> Picture
render world@(World {gameState = LevelCompleted, ..}) = 
  scale 0.5 0.5 $ translate (- (fromIntegral windowWidth)/2) (- (fromIntegral windowHeight)/2) $ 
  text "Success! Press X to continue."
render world@(World {..}) = pictures $ mapPic ++ playerPic ++ boxesPic 
  where
    mapPic = concatMap drawRow (zip [0..] (toList $ head gameMaps))-- jak lista pusta to program eksploduje, do poprawienia
    drawRow (y, row) = map (drawTile y) (zip [0..] (toList row))
    drawTile y (x, tile) = translate (translateX + tileSize * fromIntegral x) (translateY + tileSize * fromIntegral y) (drawSpecificTile tile)

    drawSpecificTile tile = case tile of
      Wall -> color black $ rectangleSolid tileSize tileSize
      Ground -> color white $ rectangleSolid tileSize tileSize
      Non -> blank
      Exit -> color yellow $ rectangleSolid tileSize tileSize

    playerPic = [ translate (translateX + tileSize * fromIntegral x) (translateY + tileSize * fromIntegral y) $ color blue $ rectangleSolid tileSize tileSize | (x, y) <- [player]]

    boxesPic = [ translate (translateX + tileSize * fromIntegral x) (translateY + tileSize * fromIntegral y) $ color red $ rectangleSolid tileSize tileSize | (x, y) <- toList boxes]