{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module Logic where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace
import System.Exit (exitSuccess)
import Game 
import Data.List (delete)
import Scores
import qualified Data.Sequence as S

type Direction = (Int, Int)

movePlayer :: Direction -> Position -> Position
movePlayer (dx, dy) (x, y) = (x + dx, y + dy)

startGame :: World -> World 
startGame world@(World {gameState=InMenu})=world {gameState=Tutorial}
startGame w = w

showHighScores :: World -> World -- do implementacji
showHighScores world@(World {gameState=InMenu})=world {gameState=HighScores}
showHighScores w = w

exitGame :: World -> World -- do implementacji
exitGame world@(World {..})=  world {gameState=Terminate}
exitGame w =  w

insertScore :: (String,Int) -> [(String,Int)] -> [(String,Int)]
insertScore s [] = [s] -- jeśli lista jest pusta, dodajemy wynik
insertScore s (x:xs)
    | snd s < snd x = s:x:xs -- jeśli nowy wynik jest większy lub równy obecnemu, wstawiamy go przed
    | otherwise      = x:insertScore s xs -- w przeciwnym razie przeszukujemy dalej


handleEventWorldIO :: Event -> World -> IO World
handleEventWorldIO (EventKey (Char c) Down _ _) world@(World {gameState = EnterName, nameEntry=name ,..}) 

  | c >= 'a' && c <= 'z' =
      let newName = (name) ++ [c] in
      return world { nameEntry = newName }
handleEventWorldIO (EventKey (SpecialKey KeyEnter) Down _ _) world@(World {gameState = EnterName, nameEntry = n, totalMoves = s, highScores = hs, ..}) =
  do
    let newScores = insertScore (n, s) hs
    saveScores newScores
    return $ initialWorld (Images {boxImage = boxPic,playerImage=moverPic}) newScores



handleEventWorldIO event world = return $ handleEventWorld event world


handleEventWorld :: Event -> World -> World
handleEventWorld  (EventKey (SpecialKey KeyEnter) Down _ _) world@(World {gameState=Tutorial,.. }) = world {gameState= Playing}
handleEventWorld  (EventKey (Char 'y') Down _ _) world@(World {gameState=GameOver,.. }) = world {gameState= EnterName}
handleEventWorld  (EventKey (Char 'n') Down _ _) world@(World {gameState=GameOver,.. }) = initialWorld (Images {boxImage = boxPic,playerImage=moverPic}) highScores
handleEventWorld (EventKey (SpecialKey KeyUp) Down _ _) world@(World { menu =  selectedButton,gameState=InMenu,.. }) =
    world { menu =  max 0 (selectedButton - 1) } -- ograniczamy do 0, aby nie wyjść poza górną granicę listy przycisków
handleEventWorld (EventKey (SpecialKey KeyDown) Down _ _) world@(World { menu =  selectedButton ,gameState=InMenu,..}) =
    world { menu = min 2 (selectedButton + 1) } -- ograniczamy do 2, aby nie wyjść poza dolną granicę listy przycisków
handleEventWorld (EventKey (SpecialKey KeyEnter) Down _ _) world@(World { menu =  selectedButton,gameState=InMenu,.. }) =
    case selectedButton of
        0 -> startGame world 
        1 -> showHighScores world 
        2 -> exitGame world 




handleEventWorld  (EventKey (Char 'b') Down _ _) world@(World {gameState=Playing,.. }) = world {gameState= Playing ,boxes=head boxesPos,player=head startPos,moves=0}
handleEventWorld (EventKey (Char 'r') Down _ _) world@(World {..}) = 
  case gameState of
    Playing -> world { gameState = InMenu }
    HighScores -> world { gameState = InMenu}
    _ -> world



handleEventWorld (EventKey (Char 'x') Down _ _) world@(World {gameState = LevelCompleted, gameMaps = maps, level = lvl,startPos=stP,boxesPos=bPos, moves = mv, totalMoves = tMv,exitsPos=expos,exits=ex, ..}) = 
  case stP of 
    ([]) -> world 
    (startingPosition:rest) -> 
      case rest of 
        []->world { exits=head expos,exitsPos=tail expos,totalMoves=(mv+tMv),moves=0, gameMaps = tail maps, player = startingPosition, boxes = head bPos ,level= (lvl+1),startPos=rest,boxesPos = tail bPos,gameState=GameOver }-- nie ma wiecej map i gra sie konczy
        _ -> world { exits=head $ tail $ expos,exitsPos=tail expos,totalMoves=(mv+tMv),moves=0,gameState =Playing, gameMaps = tail maps, player = head $ tail stP, boxes = head $ tail bPos ,level= (lvl+1),startPos=rest,boxesPos = tail bPos } 




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
                                         Just i -> checkVictory world { player = newPos, boxes = S.update i newBoxPos boxes, moves= (mv+1) }
                                  else world
                  else world { player = newPos,moves=(mv+1) }
                else world
  where
    -- Definicja funkcji checkVictory
    checkVictory :: World -> World
    checkVictory world@(World {boxes = boxes, exits = exits, ..})
      | all (`elem` boxes) exits = world { gameState = LevelCompleted }
      | otherwise = world

tileIsPassable :: Tile -> Bool
tileIsPassable Ground = True

tileIsPassable _ = False