module AA (
  AA(..),
  empty,
  isEmpty,
  insert,
  lookup,
) where

import Prelude hiding (lookup)

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

insert :: (Ord k) => k -> a -> AA k [a] -> AA k [a]
insert k v Empty = Node 0 k ([v]) Empty Empty
insert k v (Node l k' v' lAA rAA)
  | k == k' = Node l k' (v:v') lAA rAA
  | k < k' = Node l k' v' (insert k v lAA) rAA
  | k > k' = Node l k' v' lAA (insert k v rAA)

lookup :: (Ord k) => k -> AA k a -> Maybe a
lookup k Empty = Nothing
lookup k (Node l k' v' lAA rAA)
  | k == k' = Just v'
  | k < k' = lookup k lAA
  | k > k' = lookup k rAA
