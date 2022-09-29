
data Arv a = Vazia | No a (Arv a) (Arv a) deriving (Show)
--7.6
listar :: Arv a -> [a]
listar Vazia = []
listar (No x esq dir) = listar esq ++ [x] ++ listar dir

altura :: Arv a -> Int
altura Vazia          = 0
altura (No _ esq dir) = 1 + max (altura esq) (altura dir)

sumArv :: Num a => Arv a -> a
sumArv a = sum (listar a)

--7.7
listar' :: Arv a -> [a]
listar' Vazia           = []
listar' (No x esq dir)  = listar' dir ++ [x] ++ listar' esq

--7.8

--7.9

--7.10
mapArv :: (a -> b) -> Arv a -> Arv b
mapArv f Vazia          = Vazia
mapArv f (No x esq dir) = No (f x) (mapArv f esq) (mapArv f dir)

--7.11
--a
{-
  maisEsq :: Arv a -> a
  maisEsq (No x Vazia _) = x
  maisEsq (No _ esq _)   = maisEsq esq
-}

maisDir :: Arv a -> a
maisDir (No x _ Vazia)  = x
maisDir (No _ _ dir)    = maisDir dir

--b
{-
  remover :: Ord a => a -> Arv a -> Arv a
  -- não encontrou
  remover x Vazia  = Vazia
  -- um descendente
  remover x (No y Vazia dir)
      | x==y = dir
    remover x (No y esq Vazia)
      | x==y = esq
  -- dois descendentes
  remover x (No y esq dir)
      | x<y  = No y (remover x esq) dir
      | x>y  = No y esq (remover x dir)
      | x==y = let z = maisEsq dir
              in No z esq (remover z dir)
-}
remover :: Ord a => a -> Arv a -> Arv a
remover x Vazia = Vazia
remover x (No y Vazia dir)  | x == y = dir
remover x (No y esq Vazia)  | x == y = esq
remover x (No y esq dir)    | x < y  = No y (remover x esq) dir
                            | x > y  = No y esq (remover x dir)
                            | x == y = let z = maisDir esq
                                      in No z (remover z esq) dir

--7.12
