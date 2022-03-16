module Match(
  Match(..),
  Target(..),
  Guess(..),
  match,
) where

import Text.Read
import Text.ParserCombinators.ReadPrec 

data Match = Absent Char
            | Misplaced Char
            | Correct Char

data Target = Target String
data Guess = Guess String

{- Hace mas de 1 pasada (acomodar) -}
match :: Guess -> Target -> [Match]
match (Guess xs) (Target ys) =  [getMatch x y | (x,y) <- zip xs ys]
                                where getMatch x y
                                        | x == y = Correct x
                                        | x `elem` ys = Misplaced x
                                        | otherwise = Absent x

fullMatch :: [Match] -> Bool
fullMatch = all (\x -> case x of
                          Correct _ -> True
                          _ -> False)

instance Show Target where
  show (Target xs) = "It was " ++ xs

instance Show Guess where
  show (Guess xs) = "Your guess " ++ xs

instance Show Match where
  show (Correct x) = "\129001 " ++ [x]
  show (Misplaced x) = "\129000 " ++ [x]
  show (Absent x) = "\11035 " ++ [x]

instance Read Match where 
  readPrec = parens $ do
    c <- get
    case c of
      '\129001' -> do
        _ <- get
        x <- get
        return $ Correct x
      '\129000' -> do
        _ <- get
        x <- get
        return $ Misplaced x
      '\11035' -> do
        _ <- get
        x <- get
        return $ Absent x
      _ -> pfail
