type Punto2D = (Float, Float)

doble :: Integer -> Integer
doble x = x + x

f :: Integer -> Integer
f x | x == 1 = 8
    | x == 4 = 131
    | x == 16 = 16

g :: Integer -> Integer
g x | x == 8 = 16
    | x == 16 = 4
    | x == 131 = 1

h :: Integer -> Integer
h x = f (g x)

k :: Integer -> Integer
k x = g (f x)

maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 x y z | ((x >= y) && (x >= z)) = x
              | ((y >= x) && (y >= z)) = y
              | ((z >= x) && (z >= y)) = z

sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos x y z | (x /= y) && (x /= z) && (y /= z) = x + y + z
                    | (x == y) && (x == z) = x
                    | (x == y) && (x /= z) = x + z
                    | (x /= y) && (x == z) = x + y
                    | (x /= y) && (y == z) = x + y
                    | (y /= z) && (y == x) = y + z

digitoUnidades :: Integer -> Integer
digitoUnidades x = mod x 10

digitoDecenas :: Integer -> Integer
digitoDecenas x = div (mod x 100) 10

todoMenor :: (Punto2D) -> (Punto2D) -> Bool
todoMenor (a,b) (c,d) = a < c && b < d

posPrimerPar :: (Integer, Integer, Integer) -> Int
posPrimerPar (a,b,c) | esPar a = 0
                     | esPar b = 1
                     | esPar c = 2
                     | otherwise = 4

--factorial :: Integer -> Integer
factorial n | n == 0 = 1
            | otherwise = n * factorial (n-1)

valorAbsoluto :: Integer -> Integer
valorAbsoluto n | n >= 0 = n
                | n <0 = -n

esPar :: Integer -> Bool
esPar n = mod n 2 == 0

todosMenores :: (Integer, Integer, Integer) -> Bool
todosMenores (a,b,c) = (f a > g a) && (f b > g b) && (f c > g c)

type Anio = Integer
type EsBisiesto = Bool

bisiesto :: Anio -> EsBisiesto
bisiesto n = not(mod n 4 /= 0 || (mod n 100 == 0 && mod n 400 /= 0))

type Punto3D = (Integer, Integer, Integer)

distanciaManhattan :: (Punto3D) -> (Punto3D) -> Integer
distanciaManhattan (x1,y1,z1) (x2,y2,z2) = valorAbsoluto (x1 - x2) + valorAbsoluto (y1 - y2) + valorAbsoluto (z1 - z2)

comparar :: Integer -> Integer -> Integer
comparar a b | sumaUltimosDosDigitos a < sumaUltimosDosDigitos b = 1
             | sumaUltimosDosDigitos a > sumaUltimosDosDigitos b = -1
             | sumaUltimosDosDigitos a == sumaUltimosDosDigitos b = 0

sumaUltimosDosDigitos :: Integer -> Integer
sumaUltimosDosDigitos n = (mod (valorAbsoluto n) 10) + (mod (div (valorAbsoluto n) 10) 10)

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | otherwise = fibonacci (n-1) + fibonacci (n-2)

parteEntera :: Float -> Int
parteEntera n | n < 1 = 0
              | otherwise = parteEntera(n-1) + 1

esDivisible :: Integer -> Integer -> Bool
esDivisible n m | n - m == 0 = True
                | n - m < 0 = False
                | otherwise = esDivisible (n-m) m

menorDivisorAux :: Integer -> Integer -> Integer
menorDivisorAux n m | mod n m == 0 = m
                    | otherwise = menorDivisorAux n (m+1)

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorAux n 2

esPrimo :: Integer -> Bool
esPrimo n = menorDivisor n == n

nEsimoPrimoAux :: Integer -> Integer -> Integer
nEsimoPrimoAux n k  | esPrimo n && k == 1 = n
                    | esPrimo n && k /= 1 = nEsimoPrimoAux (n+1) (k-1)
                    | otherwise = nEsimoPrimoAux (n+1) k

nEsimoPrimo :: Integer -> Integer
nEsimoPrimo n = nEsimoPrimoAux n 2 

medioFact :: Integer -> Integer
medioFact 0 = 1
medioFact 1 = 1
medioFact n = n * medioFact(n-2)

cantDigitos :: Integer -> Integer
cantDigitos n   | n < 10 = 1
                        | otherwise = cantDigitos (div n 10) + 1


-- Ejercicio 6

queDigitoEs :: Integer -> Integer -> Integer
queDigitoEs n k | k == 1 = div (mod n 10) 1
                   | otherwise = div (mod n (10^k)) (10^(k-1))

todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n = queDigitoEs n (cantDigitos n) == queDigitoEs n (cantDigitos n-1)

-- Ejercicio 7

iEsimoDigito :: Integer -> Integer -> Integer
iEsimoDigito n m = div (mod n (10^(cantDigitos n - m + 1))) (10^(cantDigitos n - m))

sumaDigitos :: Integer -> Integer
sumaDigitos n   | n < 10 = n
                | otherwise = iEsimoDigito n (cantDigitos n) + sumaDigitos (div n 10)

-- Ejercicio 11 

eAprox :: Integer -> Float
eAprox 0 = 1
eAprox n = (1/(factorial (fromIntegral n))) + eAprox (n-1)

e :: Float
e = eAprox 10

-- Ejercicio 12

sucRaizDe2 :: Integer -> Float
sucRaizDe2 1 = 2
sucRaizDe2 n = 2 + (1/sucRaizDe2 (n-1))

raizDe2Aprox :: Integer -> Float
raizDe2Aprox n = (sucRaizDe2 n) - 1

-- Ejercicio 13

sumatoriaIaLaJAux :: Integer -> Integer -> Integer
sumatoriaIaLaJAux n 1 = n
sumatoriaIaLaJAux n m = (n^m) + sumatoriaIaLaJAux n (m-1)

sumatoriaIaLaJ :: Integer -> Integer -> Integer
sumatoriaIaLaJ 1 m = sumatoriaIaLaJAux 1 m
sumatoriaIaLaJ n m = sumatoriaIaLaJAux (n-1) m + sumatoriaIaLaJAux n m


-- Ejercicio 14

sumaPotenciasAux :: Integer -> Integer -> Integer -> Integer
sumaPotenciasAux q n 1 = q^(n+1)
sumaPotenciasAux q n m = sumaPotenciasAux q n (m-1) + q^(n+m)

sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q 1 m = sumaPotenciasAux q 1 m
sumaPotencias q n m = sumaPotenciasAux q (n-1) m + sumaPotenciasAux q n m

-- Recursividad con 2 parametros la idea es hacer un algoritmo aux fijando un valor en algo y despues hacer el algoritmo principal con la sumatoria(en este caso) con el otro valor fijado.





