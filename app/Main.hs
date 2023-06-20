module Main where
{-# LANGUAGE MultiWayIf #-}
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Set as S
import System.Exit (exitSuccess)
import Game 
import Logic
import Render 

updateWorld :: Float -> World -> World
updateWorld _ = id -- Placeholder

window :: Display
window = InWindow "Game" (600, 500) (0, 0)

background :: Color
background = white

fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialWorld render handleEventWorld updateWorld





