module Solve where

import AA as AA
import Util
import Match
import System.Random
import Control.Monad
import Data.Foldable (Foldable(toList))
import System.IO
import Text.Read (readMaybe)
import System.Environment
import Data.Char (toLower)
import Data.Tuple.Extra (dupe)

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
  pure $ GS { suggestion = ""
              , possible = toList dict
              , remaining = size
              , dict = dict
              , strategy = solver
              }

startSolver :: IO ()
startSolver = do
  x <- getArgs
  let arg = if length x > 1
              then "too many args"
              else if length x == 1
                then map toLower $ head x
                else ""

  case arg of
    "" -> do
      putStrLn "Naive wordle solver!"
      solveTheGame $ initialSolver Naive
    "naive" -> do
      putStrLn "Naive wordle solver!"
      solveTheGame $ initialSolver Naive
    "clever" -> do
      putStrLn "Clever wordle solver!"
      solveTheGame $ initialSolver Clever
    _ -> putStrLn "argument doesn't match \"\"|<naive>|<clever>."

  pure ()

solveTheGame :: IO SolverState -> IO ()
solveTheGame initialState = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering

  solveSession initialState 1

  putStr "Solve another "
  yn <- yesOrNo
  if yn then solveTheGame initialState else pure ()
  pure ()

solveSession :: IO SolverState -> Int -> IO ()
solveSession _ 7 = do
  putStrLn $ "You lost! " ++ emoji 7
  pure ()
solveSession solverState index = do
  putStr $ "Hint " ++ show index ++ " " ++ emoji index ++ "? "
  s <- getLine
  solSte <- solverState

  valid <- case readMaybe s :: Maybe [Match] of
              Just ms -> pure $ case AA.lookup (map toChar ms) (dict solSte) of
                                Just x  -> True
                                Nothing -> False
              Nothing -> pure False

  if not valid
    then solveSession solverState index
    else do
        let hint = read s :: [Match]
        newSolSte <- case strategy solSte of
                            Naive -> naive hint solSte
                            Clever -> clever hint solSte

        if remaining newSolSte == 1
          then do
            putStrLn $ "It must be <<" ++ suggestion newSolSte ++ ">>."
            pure ()
          else do
            putStrLn $ show newSolSte
            solveSession (pure newSolSte) (index+1)

emoji :: Int -> String
emoji index
  | index <= 4 = "\129300"
  | index == 5 = "\128533"
  | index == 6 = "\128556"
  | otherwise = "\128325"

sieve :: [Match] -> [String] -> [String]
sieve xs ys = snd $ sieve' xs ys

sieve' :: [Match] -> [String] -> (Int, [String])
sieve' xs ys = foldr (\y (acc, zs) ->
                          if isPartialMatch xs y
                            then (acc+1, y:zs)
                            else (acc, zs)) (0, []) ys

naive :: [Match] -> SolverState -> IO SolverState
naive hint (GS _ xs _ d n) = do
    let a = foldM (\acc str -> lookCand str acc hint) (1, "", []) xs
             where lookCand cand (len, s, list) hint = do
                      if isPartialMatch hint cand
                        then do
                          rnd <- randomRIO (1, len) :: IO Int
                          pure (len+1, if rnd == 1 then cand else s, cand:list)
                        else pure (len, s, list)
    (newRemaing, newSuggestion, newPossible) <- a
    pure $ GS { suggestion = newSuggestion
              , possible = newPossible
              , remaining = newRemaing-1
              , dict = d
              , strategy = n
              }

clever :: [Match] -> SolverState -> IO SolverState
clever hint ss@(GS _ possible _ _ _) = pure ss{ suggestion = newSuggestion
                                              , possible = newPossible
                                              , remaining = length newPossible
                                              }
  where
    newPossible = sieve hint possible
    words = foldr (uncurry AA.insert . dupe) AA.empty newPossible
    newSuggestion = snd $ minimum $ fmap (
        \w -> (maximum $ fmap (remainingCount w) words, w)
      ) words
      where
        remainingCount w v = length $ sieve (match (Guess w) (Target v)) newPossible

toChar :: Match -> Char
toChar x = case x of
              Absent x -> x
              Correct x -> x
              Misplaced x -> x

-- Dado un target y un match, devuelve si el match es una posible solución
isPartialMatch :: [Match] -> String -> Bool
isPartialMatch xs ys = match guess (Target ys) == xs
                      where guess = Guess $ map toChar xs

