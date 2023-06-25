module Scores where 
import Data.List.Split (splitOn)
import System.IO
import Control.Exception (catch, IOException,bracket)
import Data.Maybe
import Control.Exception (catch, IOException)

loadHighScores :: FilePath -> IO [(String, Int)]
loadHighScores path = catch loadScores handleErrors
  where
    loadScores = do
        contents <- readFile path
        return $ mapMaybe readScore $ lines contents
    readScore :: String -> Maybe (String, Int)
    readScore "" = Nothing
    readScore line = case splitOn "," line of
        [name, score] -> Just (name, read score :: Int)
        _             -> Nothing
    handleErrors :: IOException -> IO [(String, Int)]
    handleErrors _ = return []





saveScores :: [(String, Int)] -> IO ()
saveScores scores = do
    let scoreStrings = map (\(name, score) -> name ++ "," ++ show score) scores
    writeFile "highscores.txt" (unlines scoreStrings)