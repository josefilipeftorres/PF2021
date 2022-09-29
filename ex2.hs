transforma :: String -> String
transforma [] = []
transforma (x:xs) | x `elem` vogal = x : 'p' : x : transforma xs
                  | otherwise =x : transforma xs

vogal :: [Char]
vogal = ['a', 'e', 'i', 'o', 'u']


subidas :: [Float] -> Int
subidas []        = 0
subidas [_]       = 0
subidas (x:y:xs)  | x < y = 1 + subidas (y:xs)
                  | otherwise = subidas (y:xs)

--
pitagoricos :: (Num a, Eq a) => a -> a -> a -> Bool
pitagoricos a b c = a*a + b*b == c*c

hipotenusa :: Float -> Float -> Float
hipotenusa a b = sqrt (a*a + b*b)

--
diferentes :: Eq a => [a] -> [a]
diferentes []       = []
diferentes [x]      = []
diferentes (x:y:xs) | x /= y = x : diferentes (y:xs)
                    | otherwise = diferentes (y:xs)

diferentes' :: Eq a => [a] -> [a]
diferentes' xs = [x | (x,y)<-zip xs (tail xs), x /= y]

--
zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
-- zip3' [] _ _ = []
-- zip3' _ [] _ = []
-- zip3' _ _ [] = []
-- zip3' (a:as) (b:bs) (c:cs) = (a,b,c) : zip3' as bs cs
zip3' xs ys zs = [(x,y,z) | (x,(y,z))<-zip xs (zip ys zs)]

--
partir :: Eq a => a -> [a] -> ([a],[a])
-- partir x ys = (takeWhile (x /= ) ys, dropWhile (x /= ) ys)
partir _ [] = ([],[])
partir x (y:ys) | x == y = ([],y:ys)
                | otherwise = (y:ys1,ys2)
                where (ys1,ys2) = partir x ys


--
-- bloat :: a -> [[a]] -> [[[a]]]
-- bloat x  []      = [[[x]]]
-- bloat x (xs:xss) = ((x:xs):xss) : map (xs:) (bloat x xss)
--
-- parts :: [a] -> [[[a]]]
-- parts  []    = [[]]
-- parts (x:xs) = [ys | yss <- parts xs, ys <- bloat x yss]

parts :: [a] -> [[[a]]]
parts [] = [[]]
parts (x:xs) = [ [x]:ps | ps<-pss] ++ [ (x:p):ps | (p:ps)<-pss]
            where pss = parts xs
