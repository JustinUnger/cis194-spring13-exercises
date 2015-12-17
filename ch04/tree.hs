data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Eq, Show)

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node 0 Leaf v Leaf) = Node 1 (insert v Leaf) x Leaf
insert x (Node 0 l v Leaf)    = Node 1 l x (insert v Leaf) 
insert x (Node h l v r)       = Node (h+1) n x Leaf 


