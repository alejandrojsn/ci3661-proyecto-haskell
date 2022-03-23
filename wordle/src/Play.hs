module Play where

import qualified AA as AA
import Util
import Match
import System.Random 
import Control.Monad

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


pickTarget :: AA.AA String String -> IO Target
pickTarget dict = do str <- reservoirSampling dict
                     return $ Target str

reservoirSampling :: AA.AA String String -> IO String
reservoirSampling dict = fmap snd $ foldM (\(sz, curr) new -> getWord sz new curr) (1,"") dict
                          where getWord sz new curr = do r <- randomRIO (1, sz) :: IO Int
                                                         return (sz+1, if r == 1 then new else curr)

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
