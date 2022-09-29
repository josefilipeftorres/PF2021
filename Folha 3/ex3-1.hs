--3.1
divprop :: Integer -> [Integer]
divprop n = [x | x<-[1..n-1], n `mod` x == 0]

--3.2
perfeitos :: Integer -> [Integer]
perfeitos n = [x | x<-[1..n-1], sum (divprop x) == x]

--3.3
pitagoricos :: Integer -> [(Integer,Integer,Integer)]
pitagoricos n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x^2+y^2==z^2]

--3.4
primo :: Integer -> Bool
primo n = length (divprop n) == 1

--3.5
binom :: Integer -> Integer -> Integer
binom n k = (product [1..n]) `div` (product [1..k] * (product [1..n-k]))

pascal :: Integer -> [[Integer]]
pascal n = [[binom x y] | x<-[0..n], y<-[0..x]]
