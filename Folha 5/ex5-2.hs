import Data.Char
--5.9
aproxPi1 :: Int -> Double
aproxPi1 n  = sum (take n lista)
            where lista = zipWith (/) (cycle [4,-4]) [1,3..]

--aproxPi2 n  = sum (take n aprox)
            --where aprox = 3 : (zipWith (/) (cycle [4,-4])  )

--5.10
pascal :: [[Integer]]
pascal = [1] : map (\xs -> zipWith (+) ([0] ++ xs) (xs ++ [0])) pascal

--5.11
primos :: [Integer]
primos = crivo [2..]

crivo :: [Integer] -> [Integer]
crivo (p:xs) = p : crivo [x | x<-xs, x`mod`p/=0]

divprop :: Integer -> [Integer]
divprop n = [x | x<-[2..n], mod n x == 0]

isPrime :: Integer -> Bool
isPrime n = length (divprop n) == 1

goldbach :: Integer -> (Integer,Integer)
goldbach n = head [(x,y) | x<-primos, let y = n - x, isPrime y]

--5.12


--5.13
vigenere :: String -> String -> String
vigenere ks msg = intToString $ cifrar (stringToInt key) (stringToInt msg)
                where key = rep (length msg) ks

stringToInt :: String -> [Int]
stringToInt ""      = []
stringToInt (c:cs)  = (ord c - ord 'A') : stringToInt cs

intToString :: [Int] -> String
intToString []      = ""
intToString (n:ns)  = chr (n + ord 'A') : intToString ns

rep :: Int -> String -> String -- repetição da chave
rep n s = take n $ cycle s

cifrar :: [Int] -> [Int] -> [Int]
cifrar []     []      = []
cifrar (x:xs) (y:ys)  = ((x+y) `mod` 26) : cifrar xs ys
