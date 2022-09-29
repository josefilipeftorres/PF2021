
main :: IO ()

main = putStrLn "tempo"

dialogo1 :: (Int, Int, Int) -> (String, String)
dialogo1 (1,0,1)             = (amigo1 ('a', 1), amigo2 ('c', 1, 0))
dialogo1 (mins,0,totalmins)  = (amigo1 ('b', totalmins), amigo2 ('c', mins, 0))
dialogo1 (0,1,totalmins)     = (amigo1 ('b', totalmins), amigo2 ('d', 0, 1))
dialogo1 (0,horas,totalmins) = (amigo1 ('b', totalmins), amigo2 ('e', 0, horas))
dialogo1 (1,1,61)            = (amigo1 ('b', 61), amigo2 ('f', 1, 1))
dialogo1 (1,horas,totalmins) = (amigo1 ('b', totalmins), amigo2 ('g', 1, horas))
dialogo1 (mins,1,totalmins)     = (amigo1 ('b', totalmins), amigo2 ('h', mins, 1))
dialogo1 (mins,horas,totalmins) = (amigo1 ('b', totalmins), amigo2 ('i', mins, horas))

amigo1 :: (Char, Int) -> String
amigo1 ('a', 1)         = "Passou apenas 1 minuto!"
amigo1 ('b', totalmins) = "Passaram apenas " ++ show totalmins ++ " minutos!"

amigo2 :: (Char, Int, Int) -> String
amigo2 ('f', 1, 1) = "Queres dizer, 1 hora e 1 minuto?!"
amigo2 ('d', 0, 1) = "Queres dizer, 1 hora?!"
amigo2 ('c', mins, 0) = "De facto!"
amigo2 ('h', mins, 1) = "Queres dizer, 1 hora e " ++ show mins ++ " minutos?!"
amigo2 ('e', 0, horas) = "Queres dizer, " ++ show horas ++ " horas?!"
amigo2 ('g', 1, horas) = "Queres dizer, " ++ show horas ++ " horas e 1 minuto?!"
amigo2 ('i', mins, horas) = "Queres dizer, " ++ show horas ++ " horas e " ++ show mins ++ " minutos?!"

time :: Int -> Int -> Int -> Int -> (Int, Int, Int)
time h1 m1 h2 m2 = (mins, horas, totalmins)
                      where mins  = (((h2*60)+m2)-((h1*60)+m1)) `mod` 60
                            horas = (((h2*60)+m2)-((h1*60)+m1)) `div` 60
                            totalmins = ((h2*60)+m2)-((h1*60)+m1)

dialogo :: Int -> Int -> Int -> Int -> (String, String)
dialogo h1 m1 h2 m2 = dialogo1 (time h1 m1 h2 m2)
