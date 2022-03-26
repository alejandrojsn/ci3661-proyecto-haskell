{-# LANGUAGE LambdaCase #-}
module Match(
  Match(..),
  Target(..),
  Guess(..),
  match,
  fullMatch,
) where

import Text.Read (readPrec, parens, get, pfail)
import qualified AA

data Match = Absent Char
            | Misplaced Char
            | Correct Char
            deriving (Eq, Ord)

newtype Target = Target String deriving Eq
newtype Guess = Guess String

-- Devuelve el resultado de comparar un guess y un target según las reglas de Wordle
-- Se crea un árbol con key = Char, y val = Árbol de índices, en donde 
-- cada val (árbol) es una contiene los índices respectivos de Char en el Target
match :: Guess -> Target -> [Match]
match (Guess xs) (Target ys) =
        let tree = foldr (\(ch, i) acc -> case AA.lookup ch acc of
                                            Just node -> AA.insert ch (AA.insert i i node) acc
                                            Nothing -> AA.insert ch (AA.insert i i AA.empty) acc
                                              ) AA.empty $ zip ys [(0::Int)..] in
        foldr (\(ch, i) acc -> case AA.lookup ch tree of
                                 Just nodeTree -> case AA.lookup i nodeTree of
                                                    Just _ -> Correct ch : acc
                                                    Nothing -> Misplaced ch : acc
                                 Nothing -> Absent ch : acc
                              ) [] $ zip xs [(0::Int)..]


fullMatch :: [Match] -> Bool
fullMatch = all (\case
                  Correct _ -> True
                  _ -> False)

instance Show Target where
  show (Target xs) = "It was " ++ xs

instance Show Guess where
  show (Guess xs) = "Your guess " ++ xs

instance Show Match where
  show (Correct x) = "\129001" ++ [x]
  show (Misplaced x) = "\129000" ++ [x]
  show (Absent x) = "\11035" ++ [x]

instance Read Match where
  readPrec = parens $ do
    c <- get
    case c of
      '\129001' -> do
        Correct <$> get
      '\129000' -> do
        Misplaced <$> get
      '\11035' -> do
        Absent <$> get
      _ -> pfail
