--6.1
elefantes :: Int -> IO ()
elefantes n  = aux 2 n

aux :: Int -> Int -> IO ()
aux x n | x < n = do
                    frase x
                    aux (x+1) n
        | otherwise = return ()

frase :: Int -> IO ()
frase x = do
            putStrLn ("Se " ++ show x ++ " elefantes incomodam muita gente,")
            putStrLn (show (x+1) ++ " elefantes incomodam muito mais!")
