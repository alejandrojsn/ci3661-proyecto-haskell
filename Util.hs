module Util(
  turns,
  dictionary,
  fiveLetterWords,
) where 

turns :: Int
turns = 6

dictionary :: FilePath
dictionary = "/usr/share/dict/american-english"

fiveLetterWords :: [String] -> [String]
fiveLetterWords = filter (\x -> length x == 5 && ((`notElem` ['A'..'Z']) . head) x)
