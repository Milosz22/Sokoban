module Game where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Sequence as S


windowWidth :: Int 
windowWidth = 600
windowHeight :: Int 
windowHeight = 500


data GameState = Playing | LevelCompleted | InMenu | HighScores deriving (Eq, Show)
data Menu = Menu {selectedButton :: Int } deriving (Eq, Show)


data Tile = Wall | Ground | Non | Exit deriving (Eq, Show)
data Object = Player | Box deriving (Eq, Show)

type Map = S.Seq (S.Seq Tile)
type Position = (Int, Int)

data World = World { 
  gameMaps :: [Map], 
  startPos :: [Position],
  boxesPos :: [S.Seq Position],
  level :: Int ,
  player :: Position, 
  boxes :: S.Seq Position,
  gameState :: GameState,
  gameOver :: Bool,
  moves :: Int,
  totalMoves :: Int,
  menu :: Int,
  highScores :: [(String,Int)]
  } deriving (Eq, Show)

initialWorld :: [(String,Int)] -> World
initialWorld scores = World { gameMaps = [initialMap1,initialMap2], 
  player = initialPlayerPosition, 
  boxes = initialBoxPositions, 
  boxesPos=[S.fromList [(4, 3)],S.fromList [(4, 3)]] , 
  gameState=InMenu,
  startPos=[(6,1),(6,2)],
  level = 0,
  gameOver = False,
  moves =0,
  totalMoves =0,
  menu = 0,
  highScores=scores
  }
  where
    initialMap1 = S.reverse $ S.fromList $ map S.fromList $ [[b,b,w,w,w,w,b,b,b],
      [w,w,w,g,g,w,w,w,w],
      [w,e,g,g,g,g,g,g,w],
      [w,w,w,g,g,w,g,g,w],
      [b,b,w,g,g,w,g,g,w],
      [b,b,w,w,w,w,w,w,w]
      ]   
    initialMap2 = S.reverse $ S.fromList $ map S.fromList $ [[b,b,w,w,w,w,b,b,b],
      [w,w,w,g,g,w,w,w,w],
      [w,g,e,g,g,g,g,g,w],
      [w,w,w,g,g,w,g,g,w],
      [b,b,w,g,g,w,g,g,w],
      [b,b,w,w,w,w,w,w,w]
      ]    
    initialPlayerPosition = (6, 1)
    initialBoxPositions = S.fromList [(4, 3)]
    w=Wall
    g=Ground
    e=Exit
    b=Non
