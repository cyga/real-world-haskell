-- file: ch10/TreeMap.hs
instance Functor Maybe where
    fmap _ Nothing  = Nothing
    fmap f (Just x) = Just (f x)-- file: ch10/TreeMap.hs
instance Functor [] where
    fmap = map-- file: ch10/TreeMap.hs
instance Functor Tree where
    fmap = treeMap-- file: ch10/TreeMap.hs
class Functor f where
    fmap :: (a -> b) -> f a -> f b-- file: ch10/TreeMap.hs
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf a)   = Leaf (f a)
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)-- file: ch10/TreeMap.hs
treeLengths (Leaf s) = Leaf (length s)
treeLengths (Node l r) = Node (treeLengths l) (treeLengths r)-- file: ch10/TreeMap.hs
data Tree a = Node (Tree a) (Tree a)
            | Leaf a
              deriving (Show)
