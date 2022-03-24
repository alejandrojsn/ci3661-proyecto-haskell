module Play where

import qualified AA as AA
import Util
import Match
import System.Random 
import Control.Monad
import GHC.IO.Handle (hSetEcho)
import GHC.IO.Handle.FD (stdin)
import Data.Char (isLetter, toLower)
import Data.Functor ( (<&>) )
import System.IO

data GameState = GS { played :: Int
                    , won :: Int
                    , streak :: Int
                    , target :: Target
                    , dict :: AA.AA String String
                    }

initialState :: IO GameState
initialState = do
  dict <- loadDictionary dictionary
  pure $ GS 0 0 0 (Target "") dict

instance Show GameState where
  show (GS played won streak target dict) =
    "Played: " ++ show played ++ " " ++
    "Won: " ++ show won ++ " " ++
    "Lost: " ++ show (played - won) ++ " " ++
    "Streak: " ++ show streak

data Result = Win Target
            | Lose Target
          deriving Eq

instance Show Result where
  show (Win (Target t)) = "Got it! It was " ++ t ++ " \128526"
  show (Lose (Target t)) = "Bummer! It was " ++ t ++ " \128128"

playTheGame :: IO GameState -> IO ()
playTheGame gsInitial = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  dict <- loadDictionary dictionary
  target <- pickTarget dict
  putStrLn "Welcome to the game of Hangman!"
  result <- play 1 target dict
  gs <- gsInitial
  let gsNew = pure $ gs { played = played gs + 1
                          , won = if result == Win target then won gs + 1 else won gs
                          , streak = if result == Win target then streak gs + 1 else 0
                          , target = target
                          , dict = dict
                          }
  putStrLn $ show result
  gsShow <- gsNew
  putStrLn $ show gsShow
  putStr "Play again "
  yn <- yesOrNo
  if yn then playTheGame gsNew else pure ()

play :: Int -> Target -> AA.AA String String -> IO Result
play currentTurn target dict = do
  putStr $ "Guess " ++ show currentTurn ++ "? "
  hSetEcho stdin False
  guess <- readFive
  hSetEcho stdin True
  case AA.lookup guess dict of
    Nothing -> do
      putStrLn $ " Your guess \'" ++ guess ++ "\' is not a valid word!"
      play (currentTurn) target dict
    Just _ -> do
      let matching = match (Guess guess) target
      putStrLn $ " " ++ show matching
      let result = fullMatch matching
      if result 
        then pure $ Win target
        else if currentTurn == turns
          then pure $ Lose target
          else play (currentTurn+1) target dict

readFive :: IO String
readFive = readChars 5 "" <&> map toLower
  where
    readChars :: Int -> String -> IO String
    readChars 0 "" = do
      c <- getChar
      if c == '\n'
        then pure ""
        else readChars 0 ""
    readChars n "" = do
      c <- getChar
      if isLetter c
        then do
          putChar c
          --hFlush stdout
          readChars (n-1) (c:"")
        else
          readChars n ""
    readChars 0 s = do
      c <- getChar
      case c of
        '\n' -> pure s
        '\DEL' -> do
            putStr "\b \b"
            --hFlush stdout
            readChars 1 (init s)
        _ -> readChars 0 s
    readChars n s = do
      c <- getChar
      if isLetter c
        then do
          putChar c
          --hFlush stdout
          readChars (n-1) (s ++ [c])
        else
          if c == '\DEL'
            then do
              putStr "\b \b"
              --hFlush stdout
              readChars (n+1) (init s)
            else readChars n s

pickTarget :: AA.AA String String -> IO Target
pickTarget dict = do str <- reservoirSampling dict
                     pure $ Target str

reservoirSampling :: AA.AA String String -> IO String
reservoirSampling dict = fmap snd $ foldM (\(sz, curr) new -> getWord sz new curr) (1,"") dict
                          where getWord sz new curr = do r <- randomRIO (1, sz) :: IO Int
                                                         pure (sz+1, if r == 1 then new else curr)

