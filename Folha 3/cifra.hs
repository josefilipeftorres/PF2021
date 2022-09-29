import Data.Char

{-
  Funções auxiliares para conversao
  * ord :: Char -> Int
  * chr :: Int -> Char
-}

-- conversão de letras entre ['A'..'Z'] para inteiros entre [0..25]
letraInt :: Char -> Int
letraInt x = ord x - ord 'A'

intLetra :: Int -> Char
intLetra x = chr (x + ord 'A')

-- deslocar a letra k posições sendo ciclica
-- 'Z' passa para 'A', para isso usamos o mod de 26
-- Só trabalhamos com maiúsculas, se não for maiúscula mantem-se inalterada

deslocar :: Int -> Char -> Char
deslocar k x  | maiuscula x = intLetra ((letraInt x + k) `mod` 26)
              | otherwise = x

maiuscula :: Char -> Bool
maiuscula x = x >= 'A' && x <= 'Z'

-- Por fim só falta cifrar
cifrar :: Int -> String -> String
cifrar k xs = [deslocar k x | x<-xs]
