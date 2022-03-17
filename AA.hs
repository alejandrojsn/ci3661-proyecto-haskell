module AA (
  AA(..),
  empty,
  isEmpty,
  insert,
  lookup,
) where

import Prelude hiding (lookup)
import Data.Bifunctor ( Bifunctor(bimap) )

data AA k a = Empty
            | Node { lvl :: Int
            , key :: k
            , val :: a
            , lAA :: AA k a
            , rAA :: AA k a
            }
            deriving Show

mapTree :: (a -> b) -> AA k a -> AA k b
mapTree f Empty = Empty
mapTree f (Node l k v lt rt) = Node l k (f v) (mapTree f lt) (mapTree f rt)

instance Functor (AA k) where
  fmap = mapTree

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
skew (Node l k v Empty rt) = Node l k v Empty rt
skew (Node l k v lt rt) = if lvl lt == l
                          then lt { rAA = Node l k v (rAA lt) rt}
                          else Node l k v lt rt

split :: AA k a -> AA k a
split Empty = Empty
split (Node l k v lt Empty) = Node l k v lt Empty
split (Node l k v lt (Node l' k' v' lt' Empty)) = Node l k v lt (Node l' k' v' lt' Empty)
split (Node l k v lt rt) = if lvl (rAA rt) == l
                           then rt { lvl = lvl rt + 1, lAA = Node l k v lt (lAA rt) }
                           else Node l k v lt rt

insert :: (Ord k) => k -> a -> AA k a -> AA k a
insert k v Empty = Node 1 k v Empty Empty
insert k v (Node l k' v' lt rt)
  | k == k' = Node l k' v lt rt
  | k < k' = split $ skew $ Node l k' v' (insert k v lt) rt
  | k > k' = split $ skew $ Node l k' v' lt (insert k v rt)

lookup :: (Ord k) => k -> AA k a -> Maybe a
lookup k Empty = Nothing
lookup k (Node l k' v' lAA rAA)
  | k == k' = Just v'
  | k < k' = lookup k lAA
  | k > k' = lookup k rAA

checkInvariants :: AA k a -> (Bool, [ AA k a ])
checkInvariants Empty = (True, [])
checkInvariants a = if all ($a) [leafLevel, leftChildLevel, rightGrandChildLevel, twoChilds]
                    then bimap (fst left &&) (snd left ++) right
                    else (False, a : snd left ++ snd right)
  where
    left = checkInvariants (lAA a)
    right = checkInvariants (rAA a)
    leafLevel :: AA k a -> Bool
    leafLevel (Node l _ _ Empty Empty) = l == 1
    leafLevel _ = True
    leftChildLevel :: AA k a -> Bool
    leftChildLevel Empty = True
    leftChildLevel (Node _ _ _ Empty _) = True
    leftChildLevel (Node l _ _ lt _) = lvl lt < l
    rightGrandChildLevel :: AA k a -> Bool
    rightGrandChildLevel Empty = True
    rightGrandChildLevel (Node _ _ _ _ Empty) = True
    rightGrandChildLevel (Node _ _ _ _ (Node _ _ _ _ Empty)) = True
    rightGrandChildLevel (Node l _ _ _ rt) = lvl (rAA rt) < l
    twoChilds :: AA k a -> Bool
    twoChilds (Node l _ _ _ Empty) | l > 1 = False
    twoChilds (Node l _ _ Empty _) | l > 1 = False
    twoChilds _ = True
