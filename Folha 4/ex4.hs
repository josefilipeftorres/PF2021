--4.1
algarismos :: Int -> [Int]
algarismos 0 = []
algarismos x = algarismos (x `div` 10) ++ [x `mod` 10]

--4.2
toBits :: Int -> [Int]
toBits 0 = []
toBits n = toBits (n `div` 2) ++ [n `mod` 2]

--4.3
fromBits :: [Int] -> Int
fromBits [] = 0
fromBits (x:xs) = 2^t * x + fromBits xs
                where t = length (x:xs) - 1

--4.4
mdc :: Integer -> Integer -> Integer
mdc a 0 = a
mdc 0 b = b
mdc a b = mdc b (a `mod` b)

--4.5
--a
myinsert :: Ord a => a -> [a] -> [a]
myinsert n [] = [n]
myinsert n (x:xs) | n <= x = n : x : xs
                  | otherwise = x : myinsert n xs

--b
isort :: Ord a => [a] -> [a]
isort []      = []
isort (x:xs)  = myinsert x (isort xs)

--4.6
--a
myminimum :: Ord a => [a] -> a
myminimum [x] = x
myminimum (x:xs)  | x <= myminimum xs = x
                  | otherwise = myminimum xs

--b
mydelete :: Eq a => a -> [a] -> [a]
mydelete _ [] = []
mydelete n (x:xs) | n == x = xs
                  | otherwise = x : mydelete n xs

--c
ssort :: Ord a => [a] -> [a]
ssort []      = []
ssort (x:xs)  = m : ssort (mydelete m (x:xs))
                where m = myminimum (x:xs)

--4.7
--a
merges :: Ord a => [a] -> [a] -> [a]
merges xs [] = xs
merges [] xs = xs
merges (x:xs) (y:ys)  | x <= y    = x : merges xs (y:ys)
                      | otherwise = y : merges (x:xs) ys

--b
metades :: [a] -> ([a],[a])
metades xs  = (take m xs, drop m xs)
            where m = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merges metades1 metades2
          where metades1 = msort (fst (metades xs))
                metades2 = msort (snd (metades xs))
