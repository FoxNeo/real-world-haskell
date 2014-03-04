-- file: ch03/Tree.hs
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

simpleTree = Node "parent" (Node "left child" Empty Empty) (Node "right child" Empty Empty)

-- data MaybeTree a = MaybeTree a (Maybe (MaybeTree a)) (Maybe (MaybeTree a))
--                   deriving (Show)
