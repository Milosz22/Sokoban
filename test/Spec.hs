{-# LANGUAGE RecordWildCards #-}
import Logic
import Game
import Test.QuickCheck
import Graphics.Gloss
import System.Random
import qualified Data.Sequence as S
import Data.Foldable (toList)

w=Wall
g=Ground
b=Non
map1 :: Map
map1 = S.reverse $ S.fromList $ map S.fromList $ [[b,b,w,w,w,w,b,b,b],
      [w,w,w,g,g,w,w,w,w],
      [w,g,g,g,g,g,g,g,w],
      [w,w,w,g,g,w,g,g,w],
      [b,b,w,g,g,w,g,g,w],
      [b,b,w,w,w,w,w,w,w]
      ] 
map2:: Map  
map2 = S.reverse $ S.fromList $ map S.fromList $ [

    [b,b,b,b,b,w,w,w,w,b],
    [b,b,b,b,b,w,g,g,w,b],
    [w,w,w,w,w,w,g,g,w,b],
    [w,g,g,g,g,w,g,g,w,b],
    [w,g,g,g,g,g,g,g,g,w],
    [w,g,g,g,g,w,g,g,g,w],
    [w,w,w,w,w,w,w,w,w,w]
    ] 
map3:: Map
map3 = S.reverse $ S.fromList $ map S.fromList $ [
    [w,w,w,w,w,w,w],
    [w,g,g,g,g,g,w],
    [w,g,g,g,g,g,w],
    [w,g,g,g,g,g,w],
    [w,g,g,g,g,g,w],
    [w,g,w,g,w,g,w],
    [w,g,g,g,g,g,w],
    [w,g,g,g,g,g,w],
    [w,g,g,g,g,g,w],
    [w,g,g,g,g,g,w],
    [w,g,g,g,g,g,w],
    [w,g,g,g,g,g,w],
    [w,w,w,w,w,w,w]
    ] 
map4::Map
map4 = S.reverse $ S.fromList $ map S.fromList $ [
    [b,b,w,w,w,w,w,w,w,b,b],
    [b,b,w,g,g,g,g,g,w,b,b],
    [w,w,w,g,w,w,w,g,w,b,b],
    [w,g,g,g,g,g,w,g,w,b,b],
    [w,g,g,g,g,g,w,g,w,b,b],
    [w,g,g,g,g,g,w,g,w,w,b],
    [w,g,g,g,g,w,w,g,g,w,b],
    [w,w,g,g,g,g,g,g,g,w,w],
    [b,w,g,g,g,w,w,g,g,g,w],
    [b,w,g,g,g,g,w,g,g,g,w],
    [b,w,g,g,g,g,w,g,g,g,w],
    [b,w,g,g,g,g,w,w,w,w,w],
    [b,w,w,w,w,w,w,b,b,b,b]
    ] 
map5::Map
map5 = S.reverse $ S.fromList $ map S.fromList $ [
    [b,b,b,b,b,b,b,b,b,b,w,w,w,w,w,w,w],
    [b,b,b,b,b,b,b,b,b,b,w,g,g,g,g,g,w],
    [b,b,b,b,b,b,w,w,w,w,w,g,g,g,g,g,w],
    [b,b,b,b,b,b,w,g,g,g,g,g,g,g,g,g,w],
    [b,b,b,b,b,b,w,g,g,w,w,g,g,g,g,g,w],
    [b,b,b,b,b,b,w,w,g,w,w,g,g,g,g,g,w],
    [b,b,b,b,b,w,w,w,g,w,w,w,w,w,w,w,w],
    [b,b,b,b,b,w,g,g,g,g,g,w,w,b,b,b,b],
    [b,w,w,w,w,w,g,g,g,g,g,g,w,w,w,w,w],
    [w,w,g,g,g,w,g,g,g,g,g,g,w,g,g,g,w],
    [w,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,w],
    [w,w,w,w,w,w,g,g,g,g,g,g,w,w,w,w,w],
    [b,b,b,b,b,w,g,g,g,g,g,g,w,b,b,b,b],
    [b,b,b,b,b,w,w,w,w,w,w,w,w,b,b,b,b]
    ] 


instance Arbitrary GameState where
  arbitrary = elements [InMenu, Playing, LevelCompleted, EnterName, HighScores]
arbitraryMap :: Gen Map
arbitraryMap = elements [map1, map2, map3, map4, map5]

allGroundPositions :: Map -> [Position]
allGroundPositions map = 
  [(x, y) | (y, row) <- zip [0..] (toList map)
          , (x, Ground) <- zip [0..] (toList row)]

instance Arbitrary World where
  arbitrary = do
    boxPic <- return Blank
    moverPic <- return Blank
    chosenMap <- arbitraryMap
    let allPos = allGroundPositions chosenMap
    playerPos <- elements allPos
    boxPos <- elements allPos
    exitPos <- elements allPos
    gameState <- return Playing
    gameOver <- return False
    moves <- return 0
    totalMoves <- return 0
    menu <- return 0
    highScores <- return []
    nameEntry <- return " "
    return World{ boxPic=boxPic, 
    moverPic=moverPic, 
    gameMaps=[chosenMap], 
    startPos=[playerPos], 
    boxesPos=[S.fromList [boxPos]], 
    exitsPos=[S.fromList [exitPos]],
     level=0, player=playerPos, 
     boxes=S.fromList [boxPos], 
     exits=S.fromList [exitPos], 
     gameState=gameState, 
     gameOver=gameOver, 
     moves=moves, 
     totalMoves=totalMoves, 
     menu=menu, 
     highScores=highScores, 
     nameEntry=nameEntry }

getTileAtPosition :: Map -> Position -> Maybe Tile
getTileAtPosition map (x, y) = do
  row <- S.lookup y map
  S.lookup x row
test_CanPlayerMove :: World -> Bool
test_CanPlayerMove world =
  let direction = (0,1) -- Można zmienić na dowolny kierunek.
      newWorld = movePlayerInWorld direction world
      newPlayerPosition = player newWorld
      tileAtNewPosition = getTileAtPosition (head $ gameMaps newWorld) newPlayerPosition
  in case tileAtNewPosition of
       Just Ground -> True
       _ -> False


main :: IO ()
main = do 
    quickCheckWith stdArgs { maxSuccess = 1000 } test_CanPlayerMove