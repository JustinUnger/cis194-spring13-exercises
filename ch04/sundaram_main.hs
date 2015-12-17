import Data.List
import System.Environment

main = do
   args <- getArgs
   progName <- getProgName
   (arg1:arg2:xs) = args
   putStrLn arg1 ++ " " ++ arg2

s :: Integer -> Integer -> Integer
s i j = i+j+2*i*j

sList :: Integer -> [Integer]
sList n = [ s i j | i <- [1..n], j <- [1..i], (s i j) < n ]

sList'' n = [s i j | i <- [1..n], j <- [1..n], i <= j, s i j <= n ]

sList' :: Integer -> [Integer]
sList' n = takeWhile (<= n) [ s i j | i <- [1..n], j <- [1..i] ]

seive :: Integer -> [Integer]
seive n = filter (\x -> not $ x `elem` l) [1..n]
  where l = sList'' n

sundaram :: Integer -> [Integer]
sundaram n = map (\x -> 2*x+1) $ seive n

