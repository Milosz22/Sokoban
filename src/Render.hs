{-# LANGUAGE RecordWildCards #-}
module Render where
import Graphics.Gloss
import Game
import qualified Data.Sequence as S
import Data.Sequence ((><), (<|), (|>), ViewL(..), ViewR(..), viewl, viewr)
import Data.Foldable (toList)
import Scores 
import System.Exit (exitSuccess)
tileSize :: Float
tileSize = 30 

translateX :: Float 
translateX = -200
translateY :: Float 
translateY = -200

successTextX :: Float 
successTextX = -150
successTextY :: Float 
successTextY =  150




renderHighScores :: [(String, Int)] -> Picture
renderHighScores highScores =
  let
    lineSpacing = 20
    toPic (name, score) y = translate (-200) (y+300) . scale 0.15 0.15 . color black . text $ name ++ ": " ++ show score
  in pictures $ zipWith toPic highScores [0,-lineSpacing..]

renderIO :: World -> IO Picture
renderIO world@(World { gameState = InMenu, menu = menu }) = 
  return $ renderMenu world
renderIO world@(World { gameState = Terminate, menu = menu }) = do
  exitSuccess
  return $ renderMenu world
renderIO world@(World { gameState = HighScores, highScores = hs }) = 
  return $ renderHighScores hs

renderIO world@(World {gameState = LevelCompleted,gameOver=False, ..}) = 
  return $ renderLevelCompleted world

renderIO world@(World { gameState = Tutorial }) =
    return $ pictures [ tutorialText1, tutorialText2, tutorialText3,tutorialText4 , tutorialText5]
  where
    tutorialText1 = translate (-500) 200 $ scale 0.2 0.2 $ text "Move using arrows"
    tutorialText2 = translate (-500) (-50+200) $ scale 0.2 0.2 $ text "The goal is to put each box into a yellow suqare"
    tutorialText3 = translate (-500) (-100+200) $ scale 0.2 0.2 $ text "R = return to menu, B = reset level"
    tutorialText4 = translate (-500) (-150+200) $ scale 0.2 0.2 $ text "Press enter when you are ready"
    tutorialText5 = translate (-500) (-200+200) $ scale 0.2 0.2 $ text "Good luck!"

renderIO world@(World {gameState=Playing,..}) = 
  return $ renderGame world

renderIO world@(World { gameState = GameOver, nameEntry = playerName, .. }) = 
  return $ scale 0.5 0.5 $ pictures [ translate (-400) 100 $ text "Success!", translate (-1000) (-100)   $ text "To save the score press y/n"]

renderIO world@(World { gameState = EnterName, nameEntry = playerName, .. }) = 
  return $ scale 0.5 0.5 $ pictures [ translate (-1000) 100 $ text $  "Your name: "++playerName
  , translate (-1000) (-100)   $ text $  "Your score: "++show totalMoves
  , translate (-1000) (-400)   $ text $  "Press Enter"]

renderGame :: World -> Picture
renderGame world@(World {..}) = 
  pictures $ mapPic ++ exitsPic ++ playerPic ++ boxesPic ++ [movePic] 
  where
    mapPic = concatMap drawRow (zip [0..] (toList $ head gameMaps))-- jak lista pusta to program eksploduje, do poprawienia
    drawRow (y, row) = map (drawTile y) (zip [0..] (toList row))
    drawTile y (x, tile) = translate (translateX + tileSize * fromIntegral x) (translateY + tileSize * fromIntegral y) (drawSpecificTile tile)

    drawSpecificTile tile = case tile of
      Wall -> color black $ rectangleSolid tileSize tileSize
      Ground -> pictures [
        color white $ rectangleSolid tileSize tileSize,
        color black $ rectangleWire (tileSize ) (tileSize )]

      Non -> blank


    playerPic = [ translate (translateX + tileSize * fromIntegral x) (translateY + tileSize * fromIntegral y) $ moverPic | (x, y) <- [player]]

    exitsPic = [ translate (translateX + tileSize * fromIntegral x) (translateY + tileSize * fromIntegral y) $ 
      pictures [
        color yellow $ rectangleSolid (tileSize) (tileSize),
        color black $ rectangleWire (tileSize ) (tileSize )
      ]
      | (x, y) <- toList exits ]

    boxesPic = [ translate (translateX + tileSize * fromIntegral x) (translateY + tileSize * fromIntegral y) $ boxPic | (x, y) <- toList boxes]

    movePic = translate (300) (300) $ scale 0.3 0.3 $ text ("Moves: " ++ show moves)

renderLevelCompleted :: World -> Picture
renderLevelCompleted world@(World {..}) = 
  scale 0.5 0.5 $ translate (- (fromIntegral windowWidth)/2 -300) (- (fromIntegral windowHeight)/2+200) $ 
  text "Success! Press X to continue."



renderMenu :: World -> Picture
renderMenu world@(World { menu = menu }) =
  pictures  ( [buttonPic i | i <- [0..2] ])
  where
    buttonPic i =
      let (x, y) = case i of
            0 -> (-100, 200)
            1 -> (-350, 0)
            2 -> (-100, -200)
      in translate (fromIntegral x) (fromIntegral y) $ -- Przesuwamy każdy przycisk do określonego miejsca
      color (if i == menu then green else black) $
      boldText $ case i of
        0 -> "Play"
        1 -> "High Scores"
        2 -> "Exit"

boldText :: String -> Picture
boldText txt =
    pictures [
      translate dx dy $ text txt
      | dx <- [-1, 0, 1]
      , dy <- [-1, 0, 1]
    ]