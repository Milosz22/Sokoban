module Scores where 
import Data.List.Split (splitOn)
import System.IO
import Control.Exception (catch, IOException)

loadHighScores :: FilePath -> IO [(String, Int)]
loadHighScores path = catch (readFile path >>= return . map readScore . lines) handleErrors
  where
    readScore line = let (name:score:_) = splitOn "," line in (name, read score :: Int)
    handleErrors e = do 
      putStrLn $ "Error while reading file: " ++ show (e :: IOException)
      return [] -- return empty list on error