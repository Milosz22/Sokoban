module Main where
{-# LANGUAGE MultiWayIf #-}
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Set as S
import System.Exit (exitSuccess)
import Game 
import Logic
import Render 
import Scores

updateWorld :: Float -> World -> World
updateWorld _ = id -- Placeholder

window :: Display
window = InWindow "Game" (windowWidth, windowHeight) (0, 0)

background :: Color
background = white

fps :: Int
fps = 60

main :: IO ()
main = do 
    initialScores <- loadHighScores "highscores.txt"
    play window background fps (initialWorld initialScores) render handleEventWorld updateWorld





