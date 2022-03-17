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
  | k < k' = split $ skew $ Node l k' v' (split $ skew  $ insert k v lt) rt
  | k > k' = split $ skew $ Node l k' v' lt (split $ skew $ insert k v rt)

lookup' :: (Ord k) => k -> AA k a -> Maybe a
lookup' k Empty = Nothing
lookup' k (Node l k' v' lAA rAA)
    | k == k' = Just v'
    | k < k' = lookup' k lAA
    | k > k' = lookup' k rAA
    | otherwise = Nothing
