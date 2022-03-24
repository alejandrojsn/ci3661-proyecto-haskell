module Main where

import Play

main :: IO ()
main = do
  putStrLn "Kinda Wordle!"  
  playTheGame initialState
  pure ()
