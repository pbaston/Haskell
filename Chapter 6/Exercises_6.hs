
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

fac1 :: Int -> Int
fac1 0 = 1
fac1 n  | n < 0 = n * fac1 (n+1)
        | otherwise = n * fac1 (n-1)

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

(^!) :: Int -> Int -> Int
_ ^! 0 = 1
m ^! n = m * (m ^! (n-1))

euclid :: Int -> Int -> Int
euclid _ 0 = 0
euclid 0 _ = 0
euclid n m  | n == m = n
            | n > m = euclid (n-m) m
            | otherwise = euclid n (m-n)

and1 :: [Bool] -> Bool
and1 [] = True
and1 (x:xs) = x && and1 xs

concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (x:xs) = x ++ concat1 xs

replicate1 :: Int -> a -> [a]
replicate1 0 _ = []
replicate1 n m = m : replicate1 (n-1) m

(!!!) :: [a] -> Int -> a
(x:_) !!! 0  = x
(x:xs) !!! n = xs !!! (n-1)

elem1 :: Eq a => a -> [a] -> Bool
elem1 _ [] = False
elem1 y (x:xs) | x == y = True
               | otherwise = elem1 y xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

halve :: [a] -> ([a],[a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
            where
                (ys, zs) = halve xs

sum1 :: Num a => [a] -> a
sum1 [] = 0
sum1 (x:xs) = x + sum1 xs



