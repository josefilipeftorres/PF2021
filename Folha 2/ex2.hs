--2.1
classifica :: Int -> String
classifica x  | x <= 9 = "Reprovado"
              | 10 <= x && x <= 12 = "Suficiente"
              | 13 <= x && x <= 15 = "Bom"
              | 16 <= x && x <= 18 = "Muito Bom"
              | 19 <= x && x <= 20 = "Muito Bom com distincao"
              | otherwise = "Nota invalida"

--2.2
imc :: Float -> Float -> String
imc peso altura | x < 18.5 = "baixo peso"
                | 18.5 <= x && x < 25 = "peso normal"
                | 25 <= x && x < 30 = "excesso de peso"
                | 30 <= x = "obesidade"
                where x = peso / altura^2

--1.3
{-
max,min :: Ord a => a -> a -> a
max x y = if x>=y then x else y
min x y = if x<=y then x else y
-}
--a
max2, min2 :: Ord a => a -> a -> a -> a
max2 x y z  | x >= y && x >= z = x
            | x >= y && z >= x = z
            | y >= x && y >= z = y
            | y >= x && z >= y = z

min2 x y z  | x <= y && x <= z = x
            | x <= y && z <= x = z
            | y <= x && y <= z = y
            | y <= x && z <= y = z

--b
max3, min3 :: Ord a => a -> a -> a -> a
max3 x y z = max x (max y z)
min3 x y z = min x (min y z)

--2.4
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

--2.5
safetail, safetail', safetail'' :: [a] -> [a]
safetail xs = case xs of
                        [] -> []
                        (_:xs) -> xs

safetail' xs  | null xs == True = []
              | otherwise = tail xs

safetail'' [] = []
safetail'' (_:xs) = xs

--2.6
curta, curta' :: [a] -> Bool
curta xs = if length xs <= 2 then True else False
curta' [] = True
curta' [x] = True
curta' (x:y:[]) = True
curta' _ = False

--2.7
--mediana :: Int -> Int -> Int -> Int
mediana :: (Num a, Ord a) => a -> a -> a -> a
mediana a b c = (a + b + c) - (max a (max b c)) - (min a (min b c))
