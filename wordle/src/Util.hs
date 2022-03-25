module Util(
  turns,
  dictionary,
  loadDictionary,
  yesOrNo,
) where 

import qualified AA as AA
import System.IO (readFile, hSetEcho, stdin)

turns :: Int
turns = 6

dictionary :: FilePath
dictionary = "/usr/share/dict/american-english"

fiveLetterWords :: [String] -> [String]
fiveLetterWords = filter (\x -> length x == 5 && all (`elem` ['a'..'z']) x)

loadDictionary :: FilePath -> IO (AA.AA String String)
loadDictionary fp = do
  contents <- readFile fp
  pure $ foldr (\(k, v) a -> AA.insert k v a) AA.empty $ map (\x -> (x, x)) $ fiveLetterWords $ lines contents

lengthDict :: FilePath -> IO Int
lengthDict fp = do
  contents <- readFile fp
  pure $ length $ fiveLetterWords $ lines contents

yesOrNo :: IO Bool
yesOrNo = do
  putStr "(y/n)?"
  yesOrNo'

yesOrNo' :: IO Bool
yesOrNo' = do
  hSetEcho stdin False
  yn <- getChar
  hSetEcho stdin True
  case yn of
    'y' -> do
      putStrLn "y"
      pure True
    'n' -> do 
      putStrLn "n"
      pure False
    _ -> yesOrNo'
  
