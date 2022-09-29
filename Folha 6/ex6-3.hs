--6.3
main :: IO ()
main = do
          txt <- getContents
          imprime (process txt)

process :: String -> [String]
process txt = [reverse tx | tx<-(lines txt)]

imprime :: [String] -> IO ()
imprime []      = return ()
imprime (x:xs)  = do
                    putStrLn x
                    imprime xs
