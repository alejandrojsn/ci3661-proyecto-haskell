module Solve(
  initialSolver,
  solveTheGame,
  Solver(..),
) where

import qualified AA as AA
import Util (loadDictionary, dictionary, yesOrNo, turns)
import Match hiding (fullMatch) 
import System.Random (randomRIO)
import Control.Monad (foldM)
import Data.Foldable (Foldable(toList))
import System.IO (hSetBuffering, stdin, stdout, BufferMode(NoBuffering))
import Text.Read (readMaybe)
import System.Environment (getArgs)
import Data.Char (toLower)
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
  let size = length $ toList dict
  pure $ GS { suggestion = ""
              , possible = toList dict
              , remaining = size
              , dict = dict
              , strategy = solver
              }

-- Carga la informacion principal del solver, incluyendo el tipo de solver a usar
solveTheGame :: IO SolverState -> IO ()
solveTheGame defaultState = do
  x <- getArgs
  let arg = case length x of
              1 -> map toLower $ head x
              0 -> "naive"
              _ -> "too many arguments"

  putStrLn $ "There are 4594 possible words."

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

-- Llama a una sesión de solver, después de casa sesión se pregunta si desea jugar de nuevo
startSolver :: IO SolverState -> IO ()
startSolver initialState = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering

  solveSession initialState 1

  putStr "Solve another "
  yn <- yesOrNo
  if yn then startSolver initialState else pure ()

-- Sesión de solver, se interactúa con el usuario hasta que gane o se le acaben los turnos
solveSession :: IO SolverState -> Int -> IO ()
solveSession solverState index 
  | index > turns = do
    putStrLn $ "You lost! " ++ emoji 7
    pure ()
  | otherwise = do
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
                          Clever -> clever hint solSte

          case remaining newSolSte of
            1 -> do
              putStrLn $ "It must be <<" ++ suggestion newSolSte ++ ">>."
              pure ()
            _ -> do
              putStrLn $ show newSolSte
              solveSession (pure newSolSte) (index+1)

sieve :: [Match] -> [String] -> [String]
sieve xs ys = snd $ sieve' xs ys

-- Sieve pero con más información
sieve' :: [Match] -> [String] -> (Int, [String])
sieve' match words = foldr f (0, []) words
  where f word (cnt, matchedWords) = if isPartialMatch match word
                                      then (cnt+1, word:matchedWords)
                                      else (cnt  , matchedWords)

-- Estrategia ingenua donde se toma una palabra valida aleatoria de sugerencia,
-- la parte aleatoria usa reservoir sampling
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
clever hint ss@(GS _ possible _ _ _) = do
  -- O(n)
  let (newRemaining, newPossible) = sieve' hint possible

  -- O(n^2)
  let allPairs = [(x,y) | x <- newPossible, y <- newPossible]   

  -- O(n*k) en espacio: Como tenemos n palabras, y k posibles matches, todos los posibles matches 
  -- son a lo sumo (puede haber repetidos) n*k. Dónde k = 3^5 (tbf k es constante, so we can just say O(n)).
  -- O(n^2 log n) en tiempo: n^2 pares y un insert (log n) por cada par.
  -- La idea es que si la sugerencia es incorrecta, el match generado por la sugerencia y el target
  -- se guarde en el árbol. Entonces para todo x, y, asumimos que x es sugerencia y 'y' es target.
  -- y guardamos ese match junto con un string que lo genere, ese string es del target 'y'
  -- (Nótese que no importa cual string sea 'y' con tal que lo genere)
  let treeWithAllMatches = foldr (\(x,y) acc -> AA.insert (m x y) (m x y, x) acc) AA.empty allPairs
                                                where m x y = partialMatch x y   

  -- O(n^2*k) en tiempo: n*k posibles match y un sieve de O(n) por cada uno de ellos. (Again K constante = 243)
  -- Tenemos un par (match, str) por cada nodo en el árbol y guardaremos en una lista los pares (num, str)
  -- dónde num es el número de palabras que quedarían con el Guess 'str' y con el match 'match'.
  let candidates = sortBy sortLT $ foldr (\(m, str) acc -> (fst $ sieve' m newPossible, str): acc) [] treeWithAllMatches

  -- O(n^2*k*log n): Para cada para palabra str, pueden haber varios (i_0, str), ..., (i_n, str),
  -- de estos tenemos que agarrar el máximo de posible de palabras restantes (porque es el peor caso), 
  -- por lo tanto para eliminar repetidos metemos la lista candidates (ya ordenada LT) dentro de 
  -- un árbol, el razonamiento de esto es que cuando AA.insert detecta un repetido, este reemplaza el valor 
  -- viejo, y al estar la lista ordenada, el valor que quedaría en el árbol será el mayor.
  let finalTree = foldr (\(m, str) acc -> AA.insert str (m, str) acc) AA.empty candidates

  -- O(log n) Finalmente agarramos el mínimo valor en ese árbol y lo enviamos como sugerencia
  let (_, newSuggestion) = minimum finalTree
  pure $ ss { suggestion = newSuggestion
            , possible = newPossible
            , remaining = newRemaining
            }

-------------------------
-- funciones auxiliares -
-------------------------
sortLT :: (Int, String) -> (Int, String) -> Ordering
sortLT (a1, b1) (a2, b2)
  | a1 > a2 = GT
  | a1 < a2 = LT
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
  | otherwise = "\129325"