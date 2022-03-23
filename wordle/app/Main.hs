module Main where

import AA as AA
import Match
import Util
import Play

main :: IO ()
main = do
  putStrLn "Kinda Wordle!"  
  playTheGame initialState
  return ()
