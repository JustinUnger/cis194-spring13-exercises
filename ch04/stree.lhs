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

insert 3 (insert 1 (insert 2 Leaf))
insert 3 (insert 1 (Node Leaf 2 Leaf))
insert 3 (Node (insert 1 Leaf) 2 Leaf)
insert 3 (Node (Node Leaf 1 Leaf) 2 Leaf)
Node (Node Leaf 1 Leaf) 2 (insert 3 Leaf)
Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

> data Tree a = Leaf
>             | Node Integer (Tree a) a (Tree a)
>   deriving (Show, Eq)

> insert x Leaf = Node 0 Leaf x Leaf
> insert x (Node h l v r) 
>   | hl > hr   = Node h l v (insert x r)
>   | otherwise = Node (h+1) (insert x l) v r
>       where hl = height l
>             hr = height r

> height :: Tree a -> Integer
> height Leaf = -1
> height (Node h _ _ _) = h

: insert 1 Leaf
Node 0 Leaf 1 Leaf

: insert 2 (Node 0 Leaf 1 Leaf)
Node 1 (insert 2 Leaf) 1 Leaf
Node 1 (Node 0 Leaf 2 Leaf) 1 Leaf

: insert 3 (Node 1 (Node 0 Leaf 2 Leaf) 1 Leaf)
; x=3,h=1,l=(Node 0 Leaf 2 Leaf),v=1,r=Leaf
; hl=0,hr=-1 -> Node h l v (insert x r)
Node 1 (Node 0 Leaf 2 Leaf) 1 (insert 3 Leaf)
Node 1 (Node 0 Leaf 2 Leaf) 1 (Node 0 Leaf 3 Leaf)

insert 4 (Node 1 (Node 0 Leaf 2 Leaf) 1 (Node 0 Leaf 3 Leaf))
; x=4,h=1,l=(Node 0 Leaf 2 Leaf),v=1,r=(Node 0 Leaf 3 Leaf)
; hl=0,hr=0 -> Node (h+1) (insert x l) v r
Node 2 (insert 4 (Node 0 Leaf 2 Leaf)) 1 (Node 0 Leaf 3 Leaf)
; x=4,h=0,l=Leaf,v=2,r=Leaf
; hl=-1,hr=-1 -> Node (h+1) (insert x l) v r
Node 2 (Node 1 (insert 4 Leaf) 2 Leaf) 1 (Node 0 Leaf 3 Leaf)
Node 2 (Node 1 (Node 0 Leaf 4 Leaf) 2 Leaf) 1 (Node 0 Leaf 3 Leaf)

         1
        / \
       2   3
      / 
     4

insert 5 (Node 2 (Node 1 (Node 0 Leaf 4 Leaf) 2 Leaf) 1 (Node 0 Leaf 3 Leaf))
;x=5,h=2,l=(Node 1 (Node 0 Leaf 4 Leaf) 2 Leaf),v=1,r=(Node 0 Leaf 3 Leaf)
;hl=1,hr=0 -> Node h l v (insert x r)
Node 2 (Node 1 (Node 0 Leaf 4 Leaf) 2 Leaf) 1 (insert 5 (Node 0 Leaf 3 Leaf))
Node 2 (Node 1 (Node 0 Leaf 4 Leaf) 2 Leaf) 1 (insert 5 (Node 0 Leaf 3 Leaf))
; x=5,h=0,l=Leaf,v=3,r=Leaf
; hl=-1,hr=-1 -> Node (h+1) (insert x l) v r
Node 2 (Node 1 (Node 0 Leaf 4 Leaf) 2 Leaf) 1 (Node 1 (insert 5 Leaf) 3 Leaf)
Node 2 (Node 1 (Node 0 Leaf 4 Leaf) 2 Leaf) 1 (Node 1 (Node 0 Leaf 5 Leaf) 3 Leaf)

         1
        / \
       2   3
      /   /
     4   5

insert 6 (Node 2 (Node 1 (Node 0 Leaf 4 Leaf) 2 Leaf) 1 (Node 1 (Node 0 Leaf 5 Leaf) 3 Leaf))
; x=6,h=2,l=(Node 1 (Node 0 Leaf 4 Leaf) 2 Leaf),v=1,r=(Node 1 (Node 0 Leaf 5 Leaf) 3 Leaf)
; hl=1,hr=1 -> Node (h+1) (insert x l) v r
Node 3 (insert 6 (Node 1 (Node 0 Leaf 4 Leaf) 2 Leaf)) 1 (Node 1 (Node 0 Leaf 5 Leaf) 3 Leaf)
; x=6,h=1,l=(Node 0 Leaf 4 Leaf),v=2,r=Leaf
; hl=0,hr=-1 -> Node h l v (insert x r)
