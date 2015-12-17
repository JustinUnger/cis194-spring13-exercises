> data Tree a = Leaf
>             | Node Integer (Tree a) a (Tree a)
>   deriving (Show, Eq)

> insert x Leaf = Node 0 Leaf x Leaf
> insert x (Node h l v r) 
>   | hl > hr   = Node (hr'+1) l v r'
>   | otherwise = Node (hl'+1) l' v r
>       where hl = height l
>             hr = height r
>             l' = (insert x l)
>             r' = (insert x r)
>             hl' = height l' 
>             hr' = height r'

> height :: Tree a -> Integer
> height Leaf = -1
> height (Node h _ _ _ ) = h

> foldTree :: [a] -> Tree a
> foldTree [] = Leaf
> foldTree (x:xs) = insert x (foldTree xs)
