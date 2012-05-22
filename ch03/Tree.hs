-- file: ch03/Tree.hs
simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)-- file: ch03/Tree.hs
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)
