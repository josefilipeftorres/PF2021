module Main where

main :: IO ()
main = do
          word <- getLine
          putStrLn (hide word)
          putStr "? "
          c <- getChar
          adivinha word c 1

hide :: String -> String
hide []     = []
hide (x:xs) = '-' : hide xs

adivinha :: String -> Char -> Int -> IO ()
adivinha xs c cont | c `elem` xs = do
                                putStrLn (change xs c)
                                if (xs == (change xs c))
                                  then do putStrLn xs; putStrLn ("Adivinhou em " ++ show cont ++ "!!!"); return ()
                                  else do
                                        putStr "?"
                                        k <- getChar
                                        adivinha (change xs c) k (cont + 1)
                   | otherwise  = do
                                    putStrLn "Nao ocorre"
                                    putStr "?"
                                    k <- getChar
                                    adivinha (change xs c) k (cont + 1)

change :: String -> Char -> String
change (x:xs) c | x == c = c : change xs c
                | otherwise = x : change xs c
