module Util(
  turns,
  dictionary,
  fiveLetterWords,
  loadDictionary,
  yesOrNo,
  lengthDict,
) where 

import qualified AA as AA
import Control.Monad 
import Data.Char
import System.IO

turns :: Int
turns = 6

dictionary :: FilePath
dictionary = "/usr/share/dict/american-english"

fiveLetterWords :: [String] -> [String]
fiveLetterWords = filter (\x -> length x == 5 && all (`elem` ['a'..'z']) x)

loadDictionary :: FilePath -> IO (AA.AA String String)
loadDictionary fp = do
  contents <- readFile fp
  return $ foldr (\(k, v) a -> AA.insert k v a) AA.empty $ map (\x -> (x, x)) $ fiveLetterWords $ lines contents

lengthDict :: FilePath -> IO Int
lengthDict fp = do
  contents <- readFile fp
  return $ length $ fiveLetterWords $ lines contents

yesOrNo :: IO Bool
yesOrNo = do
  putStr "y/n?"
  yesOrNo'

yesOrNo' :: IO Bool
yesOrNo' = do
  hSetEcho stdin False
  yn <- getChar
  case yn of
    'y' -> do
      putStrLn "y"
      return True
    'n' -> do 
      putStrLn "n"
      return False
    _ -> yesOrNo'
  
