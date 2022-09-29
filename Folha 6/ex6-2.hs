--6.2
nLinhas, nWords, nLetras :: String -> Int
nLinhas = length . lines
nWords  = length . words
nLetras = length

main :: IO ()
main = do
          txt <- getContents
          print (nLinhas txt)
          print (nWords txt)
          print (nLetras txt)
