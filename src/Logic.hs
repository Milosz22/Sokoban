{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module Logic where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import System.Exit (exitSuccess)
import Game 
import Data.List (delete)

import qualified Data.Sequence as S

type Direction = (Int, Int)

movePlayer :: Direction -> Position -> Position
movePlayer (dx, dy) (x, y) = (x + dx, y + dy)

handleEventWorld :: Event -> World -> World
handleEventWorld (EventKey (Char 'x') Down _ _) world@(World {gameState = LevelCompleted, gameMaps = _:maps}) = 
  world { gameState = Playing, gameMaps = maps, player = startingPosition, boxes = initialBoxes }
  where
    startingPosition = (6,2) -- miejsce startowe na nowym poziomie
    initialBoxes =  S.fromList [(4, 3)]     -- początkowe pozycje skrzyń na nowym poziomie

handleEventWorld event world =
  case event of
    EventKey (SpecialKey KeyUp) Down _ _ -> movePlayerInWorld (0, 1) world
    EventKey (SpecialKey KeyDown) Down _ _ -> movePlayerInWorld (0, -1) world
    EventKey (SpecialKey KeyLeft) Down _ _ -> movePlayerInWorld (-1, 0) world
    EventKey (SpecialKey KeyRight) Down _ _ -> movePlayerInWorld (1, 0) world
    _ -> world -- Do nothing for other events

movePlayerInWorld :: Direction -> World -> World
movePlayerInWorld dir world@(World {gameState = LevelCompleted, ..}) = world
movePlayerInWorld dir world@(World {..}) =
  let newPos = movePlayer dir player
      row = S.lookup (snd newPos) (head gameMaps)
  in case row of
       Nothing -> world
       Just r -> 
         let newTile = S.lookup (fst newPos) r
         in case newTile of
              Nothing -> world
              Just t ->
                if tileIsPassable t
                then
                  if newPos `S.elemIndexL` boxes /= Nothing
                  then
                    let newBoxPos = movePlayer dir newPos
                        newBoxRow = S.lookup (snd newBoxPos) (head gameMaps)
                    in case newBoxRow of
                         Nothing -> world
                         Just br ->
                           let newBoxTile = S.lookup (fst newBoxPos) br
                           in case newBoxTile of
                                Nothing -> world
                                Just bt ->
                                  if tileIsPassable bt && newBoxPos `notElem` boxes
                                  then
                                    let boxIndex = S.findIndexL (== newPos) boxes
                                    in case boxIndex of
                                         Nothing -> world
                                         Just i -> world { player = newPos, boxes = S.update i newBoxPos boxes }
                                  else world
                  else if t == Exit
                       then world { player = newPos, gameState = LevelCompleted }
                       else world { player = newPos }
                else world

tileIsPassable :: Tile -> Bool
tileIsPassable Ground = True
tileIsPassable Exit = True
tileIsPassable _ = False