module Solve where

import AA as AA
import Util
import Match
import System.Random
import Control.Monad

data Solver = Naive | Clever
data SolverState = GS { suggestion :: String
                      , possible :: [String]
                      , remaining :: Int
                      , dict :: AA.AA String String
                      , strategy :: Solver
                      }

instance Show SolverState where
  show (GS s p r d s') = show r ++ " words remain. I suggest <<" ++ s ++ ">>."

initialSolver :: Solver -> IO SolverState
initialSolver solver = do 
  dict <- loadDictionary dictionary
  let size = foldr (\_ acc -> acc+1) 0 dict
  return $ GS { suggestion = ""
              , possible = []
              , remaining = size
              , dict = dict
              , strategy = solver
              }

sieve :: [Match] -> [String] -> [String]
sieve xs ys = filter (\y -> isPartialMatch xs y) ys

naive hint xs = fmap snd $ foldM (\acc str -> lookCandidate str acc hint) (1, "") xs
                  where lookCandidate target (len, s) hint = do 
                                                        rnd <- randomRIO (1, len) :: IO Int
                                                        if isPartialMatch hint target && rnd == 1
                                                          then return (len + 1, target)
                                                          else return (len, s)

format :: Match -> Char
format x = case x of
              Absent x -> x
              Correct x -> x
              Misplaced x -> x

-- Dado un target y un match, devuelve si el match es una posible solución
isPartialMatch :: [Match] -> String -> Bool
isPartialMatch xs ys = match guess (Target ys) == xs
                      where guess = Guess $ map format xs

