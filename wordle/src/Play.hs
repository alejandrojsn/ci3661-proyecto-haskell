module Play where

import qualified AA as AA
import Util
import Match

data GameState = GS { played :: Int
                    , won :: Int
                    , streak :: Int
                    , target :: Target
                    , dict :: AA.AA String String
                    }

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
