s :: Integer -> Integer -> Integer
s i j = i+j+2*i*j

sList :: Integer -> [Integer]
sList n = [s i j | i <- [1..n], j <- [1..n], i <= j, s i j <= n ]

seive :: Integer -> [Integer]
seive n = filter (\x -> not $ x `elem` l) [1..n]
  where l = sList n

sundaram :: Integer -> [Integer]
sundaram n = map (\x -> 2*x+1) $ seive n

