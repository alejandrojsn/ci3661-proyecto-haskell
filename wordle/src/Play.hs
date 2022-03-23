module Play where

import qualified AA as AA
import Util
import Match
import GHC.IO.Handle (hSetEcho)
import GHC.IO.Handle.FD (stdin)
import Data.Char (isLetter, toLower)
import Data.Functor ( (<&>) )

data GameState = GS { played :: Int
                    , won :: Int
                    , streak :: Int
                    , target :: Target
                    , dict :: AA.AA String String
                    }

initialState :: IO GameState
initialState = do
  dict <- loadDictionary dictionary
  return $ GS 0 0 0 (Target "") dict

instance Show GameState where
  show (GS played won streak target dict) =
    "Played: " ++ show played ++ " " ++
    "Won: " ++ show won ++ " " ++
    "Lost: " ++ show (played - won) ++ " " ++
    "Streak: " ++ show streak

data Result = Win Target
            | Lose Target

instance Show Result where
  show (Win (Target t)) = "Got it! It was " ++ t ++ " \128526"
  show (Lose (Target t)) = "Bummer! It was " ++ t ++ " \128128"

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
          readChars (n-1) (c:"")
        else
          readChars n ""
    readChars 0 s = do
      c <- getChar
      case c of
        '\n' -> pure s
        '\DEL' -> do
            putChar '\b'
            readChars 1 (init s)
        _ -> readChars 0 s
    readChars n s = do
      c <- getChar
      if isLetter c
        then do
          putChar c
          readChars (n-1) (s ++ [c])
        else
          if c == '\DEL'
            then do
              putChar '\b'
              readChars (n+1) (init s)
            else readChars n s
