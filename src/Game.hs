module Game where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


data Tile = Wall | Ground | Empty | Exit deriving (Eq, Show)
data Object = Player | Box deriving (Eq, Show)

type Map = [[Tile]]
type Position = (Int, Int)

data World = World { gameMap :: Map, player :: Position, boxes :: [Position] } deriving (Eq, Show)

initialWorld :: World
initialWorld = World { gameMap = initialMap, player = initialPlayerPosition, boxes = initialBoxPositions }
  where
    initialMap = reverse $ [[b,b,w,w,w,w,b,b,b],
      [w,w,w,g,g,w,w,w,w],
      [w,e,g,g,g,g,g,g,w],
      [w,w,w,g,g,w,g,g,w],
      [b,b,w,g,g,w,g,g,w],
      [b,b,w,w,w,w,w,w,w]
      ] 
    initialPlayerPosition = (6, 1)
    initialBoxPositions = [(4, 3)]
    w=Wall
    g=Ground
    e=Exit
    b=Empty
