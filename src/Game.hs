module Game where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Sequence as S


data Images = Images { playerImage :: Picture,boxImage :: Picture }

windowWidth :: Int 
windowWidth = 1200
windowHeight :: Int 
windowHeight = 700
background :: Color
background = greyN 0.5

data GameState = Playing | LevelCompleted | InMenu | HighScores | EnterName | GameOver | Tutorial | Terminate deriving (Eq, Show)
data Menu = Menu {selectedButton :: Int } deriving (Eq, Show)


data Tile = Wall | Ground | Non  deriving (Eq, Show)
data Object = Player | Box deriving (Eq, Show)

type Map = S.Seq (S.Seq Tile)
type Position = (Int, Int)

data World = World { 
  boxPic :: Picture,
  moverPic :: Picture,
  gameMaps :: [Map], 
  startPos :: [Position],
  boxesPos :: [S.Seq Position],
  exitsPos :: [S.Seq Position],
  level :: Int ,
  player :: Position, 
  boxes :: S.Seq Position,
  exits :: S.Seq Position,
  gameState :: GameState,
  gameOver :: Bool,
  moves :: Int,
  totalMoves :: Int,
  menu :: Int,
  highScores :: [(String,Int)],
  nameEntry :: String
  } deriving (Eq, Show)

initialWorld :: Images -> [(String,Int)] -> World
initialWorld sprites scores = World { 
  boxPic = boxImage sprites,
  moverPic = playerImage sprites,
  gameMaps = [map1,map2,map3,map4,map5], 
  player = player1, 
  boxes = boxes1,
  exits = exits1,
  exitsPos = [exits1,exits2,exits3,exits4,exits5],
  boxesPos=[boxes1,boxes2,boxes3,boxes4,boxes5],
  gameState=InMenu,
  startPos=[player1,player2,player3,player4,player5],
  level = 0,
  gameOver = False,
  moves =0,
  totalMoves =0,
  menu = 0,
  highScores=scores,
  nameEntry = " "
  }
  where
    map1 = S.reverse $ S.fromList $ map S.fromList $ [[b,b,w,w,w,w,b,b,b],
      [w,w,w,g,g,w,w,w,w],
      [w,g,g,g,g,g,g,g,w],
      [w,w,w,g,g,w,g,g,w],
      [b,b,w,g,g,w,g,g,w],
      [b,b,w,w,w,w,w,w,w]
      ]   
    map2 = S.reverse $ S.fromList $ map S.fromList $ [

      [b,b,b,b,b,w,w,w,w,b],
      [b,b,b,b,b,w,g,g,w,b],
      [w,w,w,w,w,w,g,g,w,b],
      [w,g,g,g,g,w,g,g,w,b],
      [w,g,g,g,g,g,g,g,g,w],
      [w,g,g,g,g,w,g,g,g,w],
      [w,w,w,w,w,w,w,w,w,w]
      ] 

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
    player1=(6,1)
    player2=(5,2)
    player3 = (3,9)
    player4 = (3,9)
    player5=(1,3)
    
    boxes1 = S.fromList [(4, 3),(4,2)]
    boxes2=S.fromList[(2,2),(6,3)]
    boxes3=S.fromList[(2,2+6),(2,3+6),(2,4+6),(3,4+6),(4,4+6),(4,3+6),(4,2+6),(3,2+6)]
    boxes4=S.fromList[(2,9),(4,9),(3,8),(2,7),(2,5),(2,3),(4,7),(4,5),(4,3),(3,6),(3,4),(3,2)]
    boxes5=S.fromList[(3,3),(6,3),(6,4),(7,2),(7,6),(8,6),(8,5),(8,4),(8,2),(9,6),(10,5),(10,2),(11,3),(14,3)]
    
    exits1=S.fromList[(1,3),(4,1)]
    exits2=S.fromList[(8,1),(8,2)]
    exits3 =S.fromList[(2,2),(2,3),(2,4),(3,4),(4,4),(4,3),(4,2),(3,2)]
    exits4 =S.fromList[(7,2),(7,3),(7,4),(7,5),(6,5),(8,5),(8,4),(8,3),(8,2),(9,4),(9,3),(9,2)]
    exits5=S.fromList[(13,8),(13,9),(13,10),(13,11),(13,12),
      (14,8),(14,9),(14,11),(14,12),
      (15,8),(15,9),(15,10),(15,11),(15,12)]
    
    w=Wall
    g=Ground
    b=Non
