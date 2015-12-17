import Prelude hiding (foldr, foldl)

bigList = [1..1000000]

foldr f z []     = z
foldr f z (x:xs) = x `f` foldr f z xs

sum1 = foldr (+) 0

try = sum1 bigList
