{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

import Data.Bits

-- exercise 1: use mathematical definition of fibonacci sequence to define 
-- functions that generate infinite list of fibonacci numbers

fib :: Integer -> Integer
fib n 
  | n < 2     = n
  | otherwise = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- exercise 2: write O(n) implementation of fibonacci sequence

fib' :: Integer -> Integer
fib' n = helper 1 0 n
  where helper acc prev n
         | n == 0    = prev
         | n == 1    = acc
         | otherwise = helper (acc+prev) acc (n-1)

fibs2 :: [Integer]
fibs2 = map fib' [0..]

-- exercise 3
-- define a data type of polymorphic streams
-- write a function to convert a Stream to an infinite list

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show x = show $ take 10 $ streamToList x

-- exercise 4: create some simple tools for working with Streams

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- exercise 5: use the tools we created to make a few streams

-- natural numbers: 0,1,2,...

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

-- ruler function: n'th element in stream  is largest power of 2 which evenly devices n

interleaveStream :: Stream a -> Stream a -> Stream a
interleaveStream 
   (Cons x xs) (Cons y ys) = Cons x $ Cons y $ interleaveStream xs ys

ctz :: Integer -> Integer 
ctz = iter 0 
  where iter acc x 
         | x .&. 1 == 1 = acc
         | otherwise = iter (acc + 1) (shiftR x 1)

ruler :: Stream Integer
ruler = streamMap ctz posInts
  where posInts = streamFromSeed (+ 1) 1 

-- exercise 6 

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger x = Cons x (streamRepeat 0)
  negate = streamMap negate 
  (+) (Cons x xs) (Cons y ys) = Cons (x+y) ((+) xs ys) 
  (*) (Cons x xs) b@(Cons y ys) = Cons (x*y) ((streamMap (* x) ys) + (xs * b))

instance Fractional (Stream Integer) where
  (/) (Cons x xs) (Cons y ys) = q
     where q = Cons (x `div` y) (streamMap (\x -> x `div` y) (xs - (q * ys)))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

