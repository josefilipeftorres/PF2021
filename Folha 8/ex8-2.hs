import Stack

calcular :: String -> Float
calcular str = calcularAux (words str) empty
