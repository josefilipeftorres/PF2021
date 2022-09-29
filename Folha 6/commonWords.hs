import Data.List
import Data.Char

-- função principal
-- decomposição do problema em funções
commonWords :: Int -> String -> [(String,Int)]
commonWords n = take n                -- ficar com as n primeiras
              . sortRuns              -- ordenar contagens decrescentes
              . countRuns             -- agrupar e contar
              . sort                  -- ordenar palavras
              . filter ((>3).length)  -- eliminar palavras curtas
              . words                 -- partir em palavras
              . map normalize         -- normalizar carateres

-- transformar letras em minúsculas, outros carateres em espaços
normalize :: Char -> Char
normalize x | isLetter x = toLower x
            | otherwise = ' '


-- Agrupar e contar palavras repetidas;
-- assume que a lista de palavras está ordenada,
-- logo as palavras repetidas estão seguidas
countRuns :: [String] -> [(String, Int)]
countRuns [] = []
countRuns (w:ws) = (w, 1+length ws') : countRuns ws''
  where ws' = takeWhile (==w) ws
        ws''= dropWhile (==w) ws

-- Ordenar por ordem decrescente de contagens
sortRuns :: [(String,Int)] -> [(String,Int)]
sortRuns = sortBy (\(w1,c1) (w2,c2) -> compare c2 c1)

-- Exemplo: determinar as 20 palavras mais frequentes
-- no texto de "A Cidade e as Serras" (Projeto Guttenberg 18220)
main = do
  txt <- readFile "pg18220.txt"
  sequence_ [ putStrLn (w ++ "\t" ++ show c) | (w,c) <- commonWords 20 txt]
