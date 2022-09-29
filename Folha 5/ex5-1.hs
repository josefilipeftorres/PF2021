import Data.List
--5.1
--divisores n = [d | d<-[1..n], n `mod` d == 0]
divisores :: Int -> [Int]
divisores n = filter (\x -> n `mod` x == 0) [1..n]

--5.2
primo :: Integer -> Bool
primo n | n <= 1 || any (\x -> n `mod` x == 0) [2..floor (sqrt (fromIntegral n))] = False
        | otherwise = True

--5.3
--a
juntar :: [a] -> [a] -> [a]
juntar xs ys = foldr (:) ys xs
{-
  Versão codex:
  import Prelude hiding ((++))
  (++) :: [a] -> [a] -> [a]
  xs ++ ys  = foldr f z lista
            where f = (:)
                  z = ys
                  lista = xs
-}

--b
myconcat :: [[a]] -> [a]
myconcat xs = foldr f z lista
            where f     = (++)
                  z     = []
                  lista = xs

--c && d
myreverse1, myreverse2 :: [a] -> [a]
myreverse1 xs = foldr f z xs
              where f = (\x xs -> xs ++ [x])
                    z = []

myreverse2 xs = foldl f z xs
              where f = (\x xs -> xs : x)
                    z = []

--e
myelem :: Eq a => a -> [a] -> Bool
myelem x xs = any (x ==) xs

--5.4
fromBits :: [Int] -> Int
fromBits xs = foldl f z xs
            where f = (\x xs-> xs + x * 2)
                  z = 0


--5.5
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f _ []          = []
zipWith' f [] _          = []
zipWith' f (x:xs) (y:ys) = (f x y) : (zipWith' f xs ys)

--5.6
isort :: Ord a => [a] -> [a]
isort xs  = foldr f z xs
          where f = (\x xs -> insert x xs)
                z = []

--5.7
palavras :: String -> [String]
palavras xs | null prim = []
            | otherwise = prim : palavras rest
            where prim  = takeWhile (' ' /=) (dropWhile (' ' == ) xs)
                  rest  = dropWhile (' ' /=) (dropWhile (' ' == ) xs)
                  
--5.8
{-
  scanl (+) 0 [1,2,3] = [0, 0 + 1, 0 + 1 + 2, 0 + 1 + 2 + 3]
                      = [0, 1, 3, 6]
  last (scanl f z xs) = foldl f z xs
-}
myscanl :: (b -> a -> b) -> b -> [a] -> [b]
myscanl f z []      = []
myscanl f z (x:xs)  = f z x : (myscanl f (f z x) xs)
