-- 1.1
incr, triplo :: Integer -> Integer
incr x = x + 1
triplo x = 3 * x

boasVindas :: String -> String
boasVindas nome = "Ola, " ++ nome ++ "!"

--a) 10
--b) 12
--c) "Ola, Linguagem!" ++ " Haskell" => "Ola, Linguagem! Haskell"
--d) boasVindas "Linguagem Haskell" => "Ola, Linguagem Haskell!"
--e) boasVindas ("Ola, Haskell!") => "Ola, Ola, Haskell!!"

--1.2
testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c = (a + b > c) && (a + c > b) && (b + c > a)

--1.3
areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = sqrt(s * (s - a) * (s - b) * (s-c))
                      where s = (a + b + c) / 2

--1.4 metades [1,2,3,4,5,6,7,8] = ([1,2,3,4], [5,6,7,8])
metades :: [a] -> ([a],[a])
metades xs = (take m xs, drop m xs)
           where m = length xs `div` 2

--1.5
--a
last1 xs = head (reverse xs)
last2 xs = take 1 (reverse xs)
last3 xs = xs !! (length xs - 1)
last4 xs = (reverse xs) !! 0
last5 xs = drop m xs
            where m = length xs - 1

--b
init1 xs = reverse (tail (reverse xs))
init2 xs = take m xs
          where m = length xs - 1
init3 xs = reverse (drop 1 (reverse xs))

--1.6
--a
binom, binom' :: Integer -> Integer -> Integer
binom n k = (product [1..n]) `div` (product [1..k] * (product [1..n-k]))
binom' n k | k < n - k = (product [n-k+1..n]) `div` product [1..k]
           | otherwise = (product [k+1..n]) `div` product [1..n-k]
