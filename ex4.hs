data Arv a = F | N a (Arv a) (Arv a) deriving (Show)

valor :: Arv Int -> Int
valor F = 0
valor (N h _ _) = h

alturas :: Arv a -> Arv Int
alturas F = F
alturas (N x esq dir) = N h esq' dir'
        where esq' = alturas esq
              dir' = alturas dir
              h    = 1 + max (valor esq') (valor dir')

equilibrada :: Arv a -> Bool
equilibrada = comparar . alturas

comparar :: Arv Int -> Bool
comparar F = True
comparar (N _ esq dir)  = abs (valor esq - valor dir) <= 1 &&
                        comparar esq && comparar dir
