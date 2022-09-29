--6.4
import Data.Char

minusculaToInt, maiusculaToInt :: Char -> Int
minusculaToInt c = ord c - ord 'a'
maiusculaToInt c = ord c - ord 'A'

intToMinuscula, intToMaiuscula :: Int -> Char
intToMinuscula n = chr (n + ord 'a')
intToMaiuscula n = chr (n + ord 'A')

isMaiuscula, isMinuscula :: Char -> Bool
isMinuscula c = c >= 'a' && c <= 'z'
isMaiuscula c = c >= 'A' && c <= 'Z'

rot13 :: Int -> Char -> Char
rot13 x c | isMinuscula c = intToMinuscula ((minusculaToInt c + x) `mod` 26)
          | isMaiuscula c = intToMaiuscula ((maiusculaToInt c + x) `mod` 26)
          | otherwise = c

code :: String -> String
code cs = [rot13 13 c | c<-cs]

main :: IO ()
main = do
          txt <- getContents
          putStrLn (code txt)
