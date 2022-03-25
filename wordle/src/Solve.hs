module Solve(
  initialSolver,
  solveTheGame,
  Solver(..),
) where

import qualified AA as AA
import Util
import Match
import System.Random (randomRIO)
import Control.Monad (foldM)
import Data.Foldable (Foldable(toList))
import System.IO
import Text.Read (readMaybe)
import System.Environment
import Data.Char (toLower)
import Data.Tuple.Extra (dupe)
import Data.List (sortBy)

data Solver = Naive | Clever

data SolverState = GS { suggestion :: String
                      , possible :: [String]
                      , remaining :: Int
                      , dict :: AA.AA String String
                      , strategy :: Solver
                      }

instance Show SolverState where
  show (GS s _ r _ _) = show r ++ " words remain. I suggest <<" ++ s ++ ">>."

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

solveTheGame :: IO SolverState -> IO ()
solveTheGame defaultState = do
  x <- getArgs
  let arg = case length x of
              1 -> map toLower $ head x
              0 -> "naive"
              _ -> "too many arguments"

  case arg of
    x | x == "" || x == "naive" -> do
      putStrLn "Naive wordle solver!"
      startSolver defaultState
    "clever" -> do
      putStrLn "Clever wordle solver!"
      newState <- defaultState
      startSolver $ pure $ newState { strategy = Clever}
    _ -> putStrLn "argument doesn't match \"\"|<naive>|<clever>."

  pure ()

startSolver :: IO SolverState -> IO ()
startSolver initialState = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering

  solveSession initialState 1

  putStr "Solve another "
  yn <- yesOrNo
  if yn then startSolver initialState else pure ()

solveSession :: IO SolverState -> Int -> IO ()
solveSession _ 7 = do
  putStrLn $ "You lost! " ++ emoji 7
  pure ()
solveSession solverState index = do
  putStr $ "Hint " ++ show index ++ " " ++ emoji index ++ "? "
  input <- getLine
  solSte <- solverState

  valid <- case readMaybe input :: Maybe [Match] of
              Just ms -> pure $ case AA.lookup (map toChar ms) (dict solSte) of
                                  Just x  -> True
                                  Nothing -> False
              Nothing -> pure False

  if not valid
    then solveSession solverState index
    else do
        let hint = read input :: [Match]
        newSolSte <- case strategy solSte of
                        Naive -> naive hint solSte
                        Clever -> clever' hint solSte

        case remaining newSolSte of
          1 -> do
            putStrLn $ "It must be <<" ++ suggestion newSolSte ++ ">>."
            pure ()
          _ -> do
            putStrLn $ show newSolSte
            solveSession (pure newSolSte) (index+1)

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
    (newRemaining, newSuggestion, newPossible) <- a
    pure $ GS { suggestion = newSuggestion
              , possible = newPossible
              , remaining = newRemaining-1
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


clever' :: [Match] -> SolverState -> IO SolverState
clever' hint ss@(GS _ possible _ _ _) = do
  -- O(n)
  let (newRemaining, newPossible) = sieve' hint possible

  -- O(n^2)
  let allPairs = [(x,y) | x <- newPossible, y <- newPossible, x < y]   

  -- O(n*k) en espacio: Como tenemos n palabras, y k posibles matches, todos los posibles matches 
  -- son a lo sumo (puede haber repetidos) n*k. Donde k = 3^5 (tbf k es constante, so we can just say O(n)).
  -- O(n^2 log n) en tiempo: n^2 pares y un insert (log n) por cada par.
  -- La idea es que si la sugerencia es incorrecta, el match generado por la sugerencia y el target
  -- se guarde en el arbol. Entonces para todo x, y, asumimos que x es sugerencia y 'y' es target.
  -- y guardamos ese match junto con un string que lo genere, esse string vendra del target 'y'
  -- (notese que no importa cual string 'y' sea con tal que lo genere)
  let treeWithAllMatches = foldr (\(x,y) acc -> AA.insert (m x y) (m x y, x) acc) AA.empty allPairs
                                                where m x y = partialMatch x y   

  -- O(n^2*k) en tiempo: n*k posibles match y un sieve de O(n) por cada uno de ellos. (Again K constante 243)
  -- Tenemos un par (match, str) por cada nodo en el arbol y guardaremos en una lista los pares (num, str)
  -- donde num es el numero de palabras que descartaria con el Guess 'str' y con el match 'match'.
  let candidates = sortBy sortGT $ foldr (\(m, str) acc -> (fst $ sieve' m newPossible, str): acc) [] treeWithAllMatches

  -- O(n^2*k*log n): Para cada para palabra str, pueden haber varios (i_0, str), ..., (i_n, str), de estos tenemos que agarrar 
  -- el minimo (porque es el peor caso), por lo tanto para eliminar repetidos meteremos la lista ya candidates ya ordenadas dentro de un
  -- arbol, esto por dos razones, la primera es que insert cuando detecta un repetido, reemplaza el valor viejo, y
  -- al estar la lista ordenada el valor que quedara en el arbol sera el meonor.
  let finalTree = foldr (\(m, str) acc -> AA.insert str (m, str) acc) AA.empty candidates

  -- O(log n) Finalmente agarramos el maximo valor en ese arbol y lo enviamos como sugerencia
  let (deletions, newSuggestion) = maximum finalTree
  pure $ ss { suggestion = newSuggestion
            , possible = newPossible
            , remaining = newRemaining-1
            }

sortGT :: (Int, String) -> (Int, String) -> Ordering
sortGT (a1, b1) (a2, b2)
  | a1 < a2 = GT
  | a1 > a2 = LT
  | a1 == a2 = compare b1 b2

toChar :: Match -> Char
toChar x = case x of
              Absent x -> x
              Correct x -> x
              Misplaced x -> x

-- Dado un target y un match, devuelve si el match es una posible solución
isPartialMatch :: [Match] -> String -> Bool
isPartialMatch xs ys = match guess (Target ys) == xs
                      where guess = Guess $ map toChar xs
                      
partialMatch :: String -> String -> [Match] 
partialMatch xs ys = match (Guess xs) (Target ys)


emoji :: Int -> String
emoji index
  | index <= 4 = "\129300"
  | index == 5 = "\128533"
  | index == 6 = "\128556"
  | otherwise = "\128325"