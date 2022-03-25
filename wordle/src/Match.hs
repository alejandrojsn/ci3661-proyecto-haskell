module Match(
  Match(..),
  Target(..),
  Guess(..),
  match,
  fullMatch,
) where

import Text.Read (readPrec, parens, get, pfail)
import qualified AA as AA 

data Match = Absent Char
            | Misplaced Char
            | Correct Char
            deriving (Eq, Ord)

data Target = Target String deriving Eq
data Guess = Guess String

{- Hace una sola pasada perooo asume que el arbol tiene un [..] en val -}
match :: Guess -> Target -> [Match]
match (Guess xs) (Target ys) =
        let table = foldr (\(ch, i) acc -> case AA.lookup ch acc of
                                            Just node -> AA.insert ch (AA.insert i i node) acc
                                            Nothing -> AA.insert ch (AA.insert i i AA.empty) acc
                                              ) AA.empty $ zip ys [(0::Int)..] in
        foldr (\(ch, i) acc -> case AA.lookup ch table of
                                 Just node -> case AA.lookup i node of
                                                Just _ -> Correct ch : acc
                                                Nothing -> Misplaced ch : acc
                                 Nothing -> Absent ch : acc
                              ) [] $ zip xs [(0::Int)..]
        

fullMatch :: [Match] -> Bool
fullMatch = all (\x -> case x of
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
        x <- get
        pure $ Correct x
      '\129000' -> do
        x <- get
        pure $ Misplaced x
      '\11035' -> do
        x <- get
        pure $ Absent x
      _ -> pfail
