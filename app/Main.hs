module Main where
{-# LANGUAGE MultiWayIf #-}
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Set as S
import System.Exit (exitSuccess)
import Control.DeepSeq (deepseq)
import Game 
import Logic
import Render 
import Scores



updateWorld :: Float -> World -> World
updateWorld _ = id -- Placeholder

updateWorldIO :: Float -> World -> IO World
updateWorldIO a b = return $ updateWorld a b -- Placeholder

window :: Display
window = InWindow "Game" (windowWidth, windowHeight) (0, 0)



fps :: Int
fps = 60



main :: IO ()
main = do 
    boxPic <- loadBMP "sprites/box.bmp"
    playerPic <- loadBMP "sprites/player.bmp"
    let images = Images (scale 0.075 0.075 playerPic) (scale 0.05 0.05 boxPic)
    initialScores <- loadHighScores "highscores.txt"
    initialScores `deepseq` playIO window background fps (initialWorld images initialScores) renderIO handleEventWorldIO updateWorldIO





