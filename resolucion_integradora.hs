-- Ejercicio 1.
generarStock :: [String] -> [(String, Integer)]
generarStock [] = []
generarStock (x:xs) = agregarAStock x (generarStock xs)

agregarAStock :: String -> [(String, Integer)] -> [(String, Integer)]
agregarAStock p [] = [(p, 1)]
agregarAStock p ((prod, cant):xs)   | p == prod = (prod,cant+1): xs
                                    | otherwise = (prod,cant): agregarAStock p xs

-- Ejercicio 2.
stockDeProducto :: [(String, Int)] ->String ->Int
stockDeProducto ((prod, cant):xs) p | p == prod = cant
                                    | otherwise = stockDeProducto xs p

-- Ejercicio 3.
dineroEnStock :: [(String, Int)] ->[(String, Float)] ->Float
dineroEnStock [] _ = 0.0
dineroEnStock ((prodx, cant):xs) precio = ((fromIntegral cant) * (buscaPrecio prodx precio)) + dineroEnStock xs precio


buscaPrecio :: String ->[(String, Float)] ->Float
buscaPrecio p [] = 0.0
buscaPrecio p ((prody, prec): ys) | p == prody = prec
                                  | otherwise = buscaPrecio p ys

--Ejercicio 4.
aplicarOferta :: [(String, Int)] ->[(String, Float)] ->[(String,Float)]
aplicarOferta [] _ = []
aplicarOferta ((prod, cant): xs) precio | (buscaStock prod ((prod, cant): xs)) > 10 = [(prod, (buscaPrecio prod precio) * 0.8)] ++ aplicarOferta xs precio 
                                         | otherwise = [(prod, (buscaPrecio prod precio))] ++ aplicarOferta xs precio 
 
buscaStock :: String ->[(String, Int)] ->Int
buscaStock s [] = 0
buscaStock s ((prodx, cant): xs) | s == prodx = cant
                                 | otherwise = buscaStock s xs

{--  Ejercicio 2 (2 puntos)
 Representaremos una cursada aprobada con una tupla String x Z x Z, donde:

     La primera componente de la tupla contiene el nombre de una materia
     La segunda componente de la tupla contiene el año de aprobación de la cursada
     La tercera componente de la tupla contiene el cuatrimestre de aprobación de la cursada (el valor 0 representa un curso de verano)

 Se pide implementar cursadasVencidas, que dada una lista de cursadas devuelva aquellas materias cuya aprobación de la cursada ya venció, y por lo tanto ya no se puede rendir el final

 problema cursadasVencidas (s: seq⟨String x Z x Z⟩) :seq⟨String⟩ {
   requiere: { s[i]1 ≥ 1993 para todo i tal que 0 ≤ i < |s|}
   requiere: { 0 ≤ s[i]2 ≤ 2 para todo i tal que 0 ≤ i < |s|}
   asegura: { res no tiene elementos repetidos}
   asegura: { res contiene los nombres de todas las materias incluídas en s tales que la materia fue aprobada a más tardar en el primer cuatrimestre de 2021, inclusive}
   asegura: { res contiene solamente los nombres de las materias incluídas en s tales que la materia fue aprobada a más tardar en el primer cuatrimestre de 2021, inclusive} --}

cursadasVencidas :: [(String, Integer, Integer)] -> [String]
cursadasVencidas [] = []
cursadasVencidas ((nombre, anio, cuatrimestre): xs) | anio < 2021 = nombre: cursadasVencidas xs
                                                    | anio == 2021 && (cuatrimestre == 1 || cuatrimestre == 0) = nombre: cursadasVencidas xs
                                                    | otherwise = cursadasVencidas xs 

--  Ejercicio 3 (2 puntos)

-- problema saturarEnUmbralHastaNegativo (s: seq⟨Z⟩, u: Z) : seq⟨Z⟩ {
--   requiere: {u > 0}
--   asegura: { La longitud de res es igual a la cantidad de elementos no negativos consecutivos desde el inicio de s }
--   asegura: {Para cualquier i en el rango 0 ≤ i < |res| tal que 0 ≤ s[i] ≤ u, se cumple que res[i] = s[i]}
--   asegura: {Para cualquier i en el rango 0 ≤ i < |res| tal que s[i] > u, se cumple que res[i] = u}
-- }

saturarEnUmbralHastaNegativo :: [Integer] -> Integer -> [Integer]
saturarEnUmbralHastaNegativo [] _ = []
saturarEnUmbralHastaNegativo (x:xs) _ | x < 0 = []
saturarEnUmbralHastaNegativo (x:xs) k   | x <= k = x: saturarEnUmbralHastaNegativo (umbralHastaNegativo xs) k
                                        | x > k = k: saturarEnUmbralHastaNegativo (umbralHastaNegativo xs) k

umbralHastaNegativo:: [Integer]-> [Integer]      
umbralHastaNegativo [] = []                                
umbralHastaNegativo (x:xs) | x >= 0 = x: umbralHastaNegativo xs
                           | x < 0 = []

--  Ejercicio 4 (2 puntos)

-- problema cantidadParesColumna (matriz: seq⟨seq⟨Z⟩⟩, col: Z) : Z{
--   requiere: {Todos los elementos de la secuencia matriz tienen la misma longitud}
--   requiere: {|matriz| > 0}
--   requiere: {|matriz[0]| > 0}
--   requiere: {1 ≤ col ≤ |matriz[0]| }
--   asegura: {res es la cantidad de números pares de los elementos matriz[i][col-1] para todo i tal que 0 ≤ i < |matriz| }



cantidadParesColumna :: [[Integer]] -> Integer -> Integer
cantidadParesColumna [] _ = 0
cantidadParesColumna (x:xs) k | esPar (elementoColuna x k) = 1 + cantidadParesColumna xs k
                              | otherwise = cantidadParesColumna xs k
                              where esPar :: Integer -> Bool
                                    esPar n = mod n 2 == 0

elementoColuna :: [Integer] -> Integer -> Integer
elementoColuna [] _ = 0
elementoColuna (x:xs) k | k == 1 = x
                        | otherwise = elementoColuna xs (k-1)


--  Ejercicio 1 (2,5 puntos)

-- Se dice que n es un número abundante si la suma de sus divisores propios es mayor que n. Los divisores propios de un número son todos los divisores sin contar al número mismo. Por ejemplo, los divisores propios de 12 son 1, 2, 3, 4 y 6. La suma de los divisores propios de 12 es 1 + 2 + 3 + 4 + 6 = 16, que es mayor que 12. Por lo tanto, 12 es un número abundante.

-- Se pide implementar cantidadNumerosAbundantes:

-- problema cantidadNumerosAbundantes (d: Z,h: Z) : Z {
--   requiere: {0 < d ≤ h}
--   asegura: {res es la cantidad de números abundantes en el rango [d..h]}
-- }

cantidadNumerosAbundantes :: Integer -> Integer -> Integer
cantidadNumerosAbundantes n k | n > k = 0
                              | n == k && esAbundante n = 1
                              | esAbundante n = 1 + cantidadNumerosAbundantes (n+1) k
                              | otherwise = cantidadNumerosAbundantes (n+1) k

esAbundante:: Integer-> Bool
esAbundante n = sumaDivisores n (n-1)> n

sumaDivisores:: Integer -> Integer-> Integer
sumaDivisores 1 _ = 1
sumaDivisores _ 1 = 1S
sumaDivisores n k   | mod n k == 0 && k /= n = k + sumaDivisores n (k-1)
                    | otherwise = sumaDivisores n (k-1)
