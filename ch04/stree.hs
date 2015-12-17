data Tree = Leaf
          | Node Tree Int Tree
  deriving (Show)

insert :: Int -> Tree -> Tree
insert x Leaf = Node Leaf x Leaf
insert x (Node l v r) 
  | x < v     = Node (insert x l) v r
  | otherwise = Node l v (insert x r) 

build :: [Int] -> Tree
build [] = Leaf
build (x:xs) = insert x (build xs) 
