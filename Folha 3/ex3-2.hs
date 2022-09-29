import Data.Char
--3.7
--a
myand :: [Bool] -> Bool
myand [] = True
myand (False:_) = False
myand (True:xs) = myand xs

--b
myor :: [Bool] -> Bool
myor [] = False
myor (True:_)   = True
myor (False:xs) = myor xs

--c
myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

--d
myreplicate :: Int -> a -> [a]
myreplicate n x | n <= 0 = []
                | otherwise = x : myreplicate (n-1) x

--e
selec :: [a] -> Int -> a
selec _ n   | n < 0 = error "*** Exception: Prelude.!!: negative index"
selec xs n  | n > length xs - 1 = error "*** Exception: Prelude.!!: index too large"
selec (x:xs) n  | n == 0 = x
                | otherwise = selec xs (n-1)

--f
myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem n (x:xs) | n == x = True
                | otherwise = myelem n xs

--3.8
myconcat2 :: [[a]] -> [a]
myconcat2 xss = [x | xs<-xss, x<-xs]

myreplicate2 :: Int -> a -> [a]
myreplicate2 n x = [x | _<-[1..n]]

-- selec2 :: [a] -> Int -> a
-- selec2 xs n | n < 0 || n > length xs - 1 = error "Sike"
-- selec2 xs n = [x | (x,i)<-zip xs [0..t], i == n]
--             where t = length xs - 1

--3.9
teste1, teste2, teste3, teste4 :: String -> Bool
teste1 xs = length xs >= 8
teste2 xs = or [isUpper x | x<-xs]
teste3 xs = or [isLower x | x<-xs]
teste4 xs = or [isDigit x | x<-xs]

forte :: String -> Bool
forte xs = teste1 xs && teste2 xs && teste3 xs && teste4 xs

--3.10
--a
mindiv :: Int -> Int
mindiv n  | null d = n
          | otherwise = head d
          where d = [x | x<-[2..floor (sqrt (fromIntegral n))], n `mod` x == 0]
--b
primalidade :: Int -> Bool
primalidade n = n > 1 && mindiv n == n

--3.11
remove :: Eq a => a -> [a] -> [a]
remove n [] = []
remove n (x:xs) | n == x = remove x xs
                | otherwise = x : (remove n xs)
mynub :: Eq a => [a] -> [a]
mynub [] = []
mynub (x:xs) = x : mynub (remove x xs)

--3.12
myintersperse :: a -> [a] -> [a]
myintersperse _ [] = []
myintersperse _ [x] = [x]
myintersperse n (x:xs) = x : n : (myintersperse n xs)
