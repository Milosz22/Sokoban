module Scores where 
import Data.List.Split (splitOn)
import System.IO
import Control.Exception (catch, IOException,bracket)
import Data.Maybe

loadHighScores :: FilePath -> IO [(String, Int)]
loadHighScores path = do
    contents <- readFile path
    return $ mapMaybe readScore $ lines contents
  where
    readScore :: String -> Maybe (String, Int)
    readScore "" = Nothing
    readScore line = case splitOn "," line of
        [name, score] -> Just (name, read score :: Int)
        _             -> Nothing

saveScore :: String -> Int -> IO ()
saveScore name score = do
   appendFile "highscores.txt" (name ++ "," ++ show score ++ "\n")
