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
handleEventWorld (EventKey (Char 'x') Down _ _) world@(World {gameState = LevelCompleted, gameMaps = maps, level = lvl,startPos=stP,boxesPos=bPos, moves = mv, totalMoves = tMv, ..}) = 
  case stP of 
    ([]) -> world 
    (startingPosition:rest) -> 
      case rest of 
        []->world { totalMoves=(mv+tMv),moves=0,gameState = LevelCompleted, gameMaps = tail maps, player = startingPosition, boxes = head bPos ,level= (lvl+1),startPos=rest,boxesPos = tail bPos,gameOver=True }-- nie ma wiecej map i gra sie konczy
        _ -> world { totalMoves=(mv+tMv),moves=0,gameState =Playing, gameMaps = tail maps, player = startingPosition, boxes = head bPos ,level= (lvl+1),startPos=rest,boxesPos = tail bPos } 




handleEventWorld (EventKey (SpecialKey KeyUp) Down _ _) world@(World {gameState = Playing, ..}) = movePlayerInWorld (0,1) world
handleEventWorld (EventKey (SpecialKey KeyDown) Down _ _) world@(World {gameState = Playing, ..}) = movePlayerInWorld (0,-1) world
handleEventWorld (EventKey (SpecialKey KeyLeft) Down _ _) world@(World {gameState = Playing, ..}) = movePlayerInWorld (-1,0) world
handleEventWorld (EventKey (SpecialKey KeyRight) Down _ _) world@(World {gameState = Playing, ..}) = movePlayerInWorld (1,0) world
handleEventWorld _ world = world  -- In case of other events

movePlayerInWorld :: Direction -> World -> World
movePlayerInWorld dir world@(World {gameState = LevelCompleted, ..}) = world
movePlayerInWorld dir world@(World {moves=mv,..}) =

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
                                         Just i -> world { player = newPos, boxes = S.update i newBoxPos boxes, moves= (mv+1) }
                                  else world
                  else if t == Exit
                       then world { player = newPos, gameState = LevelCompleted ,moves= (mv+1)}
                       else world { player = newPos,moves=(mv+1) }
                else world

tileIsPassable :: Tile -> Bool
tileIsPassable Ground = True
tileIsPassable Exit = True
tileIsPassable _ = False