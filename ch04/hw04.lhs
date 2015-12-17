exercise 3: more folds

> xor :: [Bool] -> Bool
> xor = odd . length . filter (==True) 

> xor' :: [Bool] -> Bool
> xor' = foldr (\x acc -> if x == True then not acc else acc) False

> map' :: (a -> b) -> [a] -> [b]
> map' f = foldr (\x acc -> f x : acc) [] 

> --foldl' :: (a -> b -> a) -> a -> [b] -> a
