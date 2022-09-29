{-
  Defina uma função textual :: Int -> String para
  converter um número positivo inferior a um milhão para
  a designação textual em Português.
-}

{-
  A função 'converte2' é composição de duas:
  * 'divide2' obtêm os algarismos;
  * 'combina2' combina o texto de cada algarismo.
  Usamos as operações de concatenação (++) e
  indexação de listas (!!) (note que os índices começam em zero.)
-}

converte2 :: Int -> String
converte2 n | n < 100 = combina2 (divide2 n)

divide2 :: Int -> (Int,Int) -- dezenas e unidades
divide2 n = (n `div` 10, n `mod` 10)

combina2 :: (Int,Int) -> String
combina2 (0, u) = unidades !! u
combina2 (1, u) = dez_a_dezanove !! u
combina2 (d, 0) = dezenas !! (d-2)
combina2 (d, u) = dezenas !! (d-2) ++ " e " ++ unidades !! u

{-
  Same shit para numeros de 3 algarismos
-}

converte3 :: Int -> String
converte3 n = combina3 (divide3 n)

divide3 :: Int -> (Int,Int) -- centenas e o restante (<100)
divide3 n = (n `div` 100, n `mod` 100)

combina3 :: (Int,Int) -> String
combina3 (0, r) = converte2 r   -- caso anterior
combina3 (1, 0) = "cem"
combina3 (c, 0) = centenas !! (c - 1)
combina3 (c, r) = centenas !! (c - 1) ++ " e " ++ converte2 r

{-
  Dá para roubar na quantidade de passos e fazer logo para 6 algarismos
-}

converte6 :: Int -> String -- numeros < 1 milhão
converte6 n = combina6 (divide6 n)

divide6 :: Int -> (Int,Int)
divide6 n = (n `div` 1000, n `mod` 1000) -- milhares e restante

combina6 :: (Int,Int) -> String
combina6 (0, r) = converte3 r   -- caso anterior
combina6 (1, 0) = "mil"
combina6 (1, r) = "mil e " ++ converte3 r
combina6 (m, 0) = converte3 m ++ " mil"
combina6 (m, r) = converte3 m ++ " mil" ++ key ++ converte3 r
    where key -- pode ser " e " ou " "
            | r < 100 || r `mod` 100 == 0 = " e "
            | otherwise                   = " "

{-
  El toque final
-}

converte :: Int -> String
converte = converte6

{-
--------------------------------------------------------------------------------
--------------------------------Tabela de nomes---------------------------------
--------------------------------------------------------------------------------
-}

unidades :: [String]
unidades = ["zero", "um", "dois", "tres", "quatro", "cinco", "seis", "sete", "oito", "nove"]

dezenas :: [String]
dezenas = ["vinte", "trinta", "quarenta", "cinquenta", "sessenta", "setenta", "oitenta", "noventa"]

dez_a_dezanove :: [String]
dez_a_dezanove = ["dez", "onze", "doze", "treze", "quatorze", "quinze", "dezasseis", "dezassete", "dezoito", "dezanove"]

centenas :: [String]
centenas = ["cento", "duzentos", "trezentos", "quatrocentos", "quinhentos", "seiscentos", "setecentos", "oitocentos", "novecentos"]
