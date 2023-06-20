{-# LANGUAGE MultiWayIf #-}

module Logic where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Set as S
import System.Exit (exitSuccess)
import Game 
import Data.List (delete)


type Direction = (Int, Int)

movePlayer :: Direction -> Position -> Position
movePlayer (dx, dy) (x, y) = (x + dx, y + dy)


handleEventWorld :: Event -> World -> World
handleEventWorld event world = 
  case event of
    EventKey (SpecialKey KeyUp) Down _ _ -> movePlayerInWorld (0, 1) world
    EventKey (SpecialKey KeyDown) Down _ _ -> movePlayerInWorld (0, -1) world
    EventKey (SpecialKey KeyLeft) Down _ _ -> movePlayerInWorld (-1, 0) world
    EventKey (SpecialKey KeyRight) Down _ _ -> movePlayerInWorld (1, 0) world
    _ -> world -- Do nothing for other events

movePlayerInWorld :: Direction -> World -> World
movePlayerInWorld dir world =
  let newPos = movePlayer dir (player world)
      newTile = (gameMap world) !! (snd newPos) !! (fst newPos)
  in if tileIsPassable newTile
     then 
        if newPos `elem` (boxes world)
        then let newBoxPos = movePlayer dir newPos
                 newBoxTile = (gameMap world) !! (snd newBoxPos) !! (fst newBoxPos)
             in if tileIsPassable newBoxTile && notElem newBoxPos (boxes world)
                then world { player = newPos, boxes = newBoxPos : (delete newPos (boxes world)) }
                else world
        else world { player = newPos }
     else world

tileIsPassable :: Tile -> Bool
tileIsPassable Ground = True
tileIsPassable Exit = True
tileIsPassable _ = False
