module AA (
  AA(..),
  empty,
  isEmpty,
  insert,
  lookup,
) where

import Prelude hiding (lookup)
import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Either (isRight, lefts, fromLeft)

-- Árbol AA
data AA k a = Empty
            | Node { lvl :: Int
            , key :: k
            , val :: a
            , lAA :: AA k a
            , rAA :: AA k a
            }
            deriving Show

-- Función equivalente a map, pero en un árbol
mapTree :: (a -> b) -> AA k a -> AA k b
mapTree f Empty = Empty
mapTree f (Node l k v lt rt) = Node l k (f v) (mapTree f lt) (mapTree f rt)

instance Functor (AA k) where
  fmap = mapTree

-- Función equivalente a foldr, pero en un árbol
foldrA :: (a -> b -> b) -> b -> AA k a -> b
foldrA f b Empty = b
foldrA f b (Node _ _ v lt rt) = foldrA f (f v (foldrA f b rt)) lt

instance Foldable (AA k) where
  foldr = foldrA

empty :: AA k a
empty = Empty

isEmpty :: AA k a -> Bool
isEmpty Empty = True
isEmpty _ = False

skew :: AA k a -> AA k a
skew Empty = Empty
skew n@(Node _ _ _ Empty _) = n
skew n@(Node l _ _ lt _) = if lvl lt == l
                         then lt { rAA = n { lAA = rAA lt } }
                         else n

split :: AA k a -> AA k a
split Empty = Empty
split n@(Node _ _ _ _ Empty) = n
split n@(Node _ _ _ _ (Node _ _ _ _ Empty)) = n
split n@(Node l _ _ _ rt) = if lvl (rAA rt) == l
                          then rt { lvl = lvl rt + 1
                                  , lAA = n { rAA = lAA rt }
                                  }
                          else n

insert :: (Ord k) => k -> a -> AA k a -> AA k a
insert k v Empty = Node 1 k v Empty Empty
insert k v n@(Node _ k' _ lt rt)
  | k == k' = n { val = v }
  | k <  k' = split $ skew $ n { lAA = insert k v lt }
  | k  > k' = split $ skew $ n { rAA = insert k v rt }
  | otherwise = undefined

lookup :: (Ord k) => k -> AA k a -> Maybe a
lookup k Empty = Nothing
lookup k (Node _ k' v lt rt)
  | k == k' = Just v
  | k <  k' = lookup k lt
  | k  > k' = lookup k rt
  | otherwise = undefined

newtype ValidAA k a = Valid (AA k a) deriving Show

data InvariantError k a = WrongLeafLevel (AA k a)
                        | WrongLeftChildLevel (AA k a)
                        | WrongRightChildLevel (AA k a)
                        | WrongRightGrandchildLevel (AA k a)
                        | WrongChildCount (AA k a)
                        deriving Show

type AAValidity k a = Either (InvariantError k a) (ValidAA k a)

checkInvariants :: AA k a -> Either [InvariantError k a] (ValidAA k a)
checkInvariants Empty = Right $ Valid Empty
checkInvariants n = if all isRight errors && all isRight childErrors
                  then Right $ Valid n
                  else Left $ foldMap (fromLeft []) childErrors ++ lefts errors
  where
    errors = map ($n) [leafLevel, leftChildLevel, rightChildLevel, rightGrandchildLevel, twoChilds]
    leftTree = checkInvariants (lAA n)
    rightTree = checkInvariants (rAA n)
    childErrors = [leftTree, rightTree]
    leafLevel :: AA k a -> AAValidity k a
    leafLevel n@(Node l _ _ Empty Empty) = if l == 1
                                         then Right $ Valid n
                                         else Left $ WrongLeafLevel n
    leafLevel n = Right $ Valid n
    leftChildLevel :: AA k a -> AAValidity k a
    leftChildLevel Empty = Right $ Valid Empty
    leftChildLevel n@(Node _ _ _ Empty _) = Right $ Valid Empty
    leftChildLevel n@(Node l _ _ lt _) = if lvl lt < l
                                       then Right $ Valid n
                                       else Left $ WrongLeftChildLevel n
    rightChildLevel :: AA k a -> AAValidity k a
    rightChildLevel Empty = Right $ Valid Empty
    rightChildLevel n@(Node _ _ _ _ Empty) = Right $ Valid Empty
    rightChildLevel n@(Node l _ _ _ lt) = if lvl lt <= l
                                        then Right $ Valid n
                                        else Left $ WrongRightChildLevel n
    rightGrandchildLevel :: AA k a -> AAValidity k a
    rightGrandchildLevel Empty = Right $ Valid Empty
    rightGrandchildLevel n@(Node _ _ _ _ Empty) = Right $ Valid n
    rightGrandchildLevel n@(Node _ _ _ _ (Node _ _ _ _ Empty)) = Right $ Valid n
    rightGrandchildLevel n@(Node l _ _ _ rt) = if lvl (rAA rt) < l
                                             then Right $ Valid n
                                             else Left $ WrongRightGrandchildLevel n
    twoChilds :: AA k a -> AAValidity k a
    twoChilds n@(Node l _ _ _ Empty) | l > 1 = Left $ WrongChildCount n
    twoChilds n@(Node l _ _ Empty _) | l > 1 = Left $ WrongChildCount n
    twoChilds n = Right $ Valid n
