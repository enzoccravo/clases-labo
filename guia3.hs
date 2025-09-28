ype Punto2D = (Float, Float)

doble :: Int -> Int
doble x = x + x

f :: Int -> Int
f x | x == 1 = 8
    | x == 4 = 131
    | x == 16 = 16

g :: Int -> Int
g x | x == 8 = 16
    | x == 16 = 4
    | x == 131 = 1

h :: Int -> Int
h x = f (g x)

k :: Int -> Int
k x = g (f x)

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | ((x >= y) && (x >= z)) = x
              | ((y >= x) && (y >= z)) = y
              | ((z >= x) && (z >= y)) = z

sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos x y z | (x /= y) && (x /= z) && (y /= z) = x + y + z
                    | (x == y) && (x == z) = x
                    | (x == y) && (x /= z) = x + z
                    | (x /= y) && (x == z) = x + y
                    | (x /= y) && (y == z) = x + y
                    | (y /= z) && (y == x) = y + z

digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10

digitoDecenas :: Int -> Int
digitoDecenas x = div (mod x 100) 10

todoMenor :: (Punto2D) -> (Punto2D) -> Bool
todoMenor (a,b) (c,d) = a < c && b < d

posPrimerPar :: (Int, Int, Int) -> Int
posPrimerPar (a,b,c) | esPar a = 0
                     | esPar b = 1
                     | esPar c = 2
                     | otherwise = 4

factorial :: Int -> Int
factorial n | n == 0 = 1
            | otherwise = n * factorial (n-1)

parteEntera :: Float -> Int
parteEntera n | n < 1 = 0
              | otherwise = parteEntera (n-1) + 1
--el +1 suma la cantidad de veces que la funcion parteEntera es llamada, por lo tanto el +1 va a ser llamado n veces.

valorAbsoluto :: Int -> Int
valorAbsoluto n | n >= 0 = n
                | n <0 = -n

esPar :: Int -> Bool
esPar n = mod n 2 == 0

f :: Int -> Int 
f n | n <= 7 = n^2
    | otherwise = 2*n - 1

g :: Int -> Int
g n | esPar n = div n 2
    | otherwise = 3 * n + 1

todosMenores :: (Int, Int, Int) -> Bool
todosMenores (a,b,c) = (f a > g a) && (f b > g b) && (f c > g c)

type Anio = Integer
type EsBisiesto = Bool

bisiesto :: Anio -> EsBisiesto
bisiesto n = not(mod n 4 /= 0 || (mod n 100 == 0 && mod n 400 /= 0))

type Punto3D = (Int, Int, Int)

distanciaManhattan :: (Punto3D) -> (Punto3D) -> Int
distanciaManhattan (x1,y1,z1) (x2,y2,z2) = valorAbsoluto (x1 - x2) + valorAbsoluto (y1 - y2) + valorAbsoluto (z1 - z2)

--Ejercicio 8.
comparar :: Int -> Int -> Int
comparar a b | sumaUltimosDosDigitos a < sumaUltimosDosDigitos b = 1
             | sumaUltimosDosDigitos a > sumaUltimosDosDigitos b = -1
             | sumaUltimosDosDigitos a == sumaUltimosDosDigitos b = 0

sumaUltimosDosDigitos :: Int -> Int
sumaUltimosDosDigitos n = (mod (valorAbsoluto n) 10) + (mod (div (valorAbsoluto n) 10) 10)

fibonacci :: Int -> Int
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | otherwise = fibonacci (n-1) + fibonacci (n-2)

parteEntera :: Float -> Int
parteEntera n | n < 1 = 0
              | otherwise = parteEntera(n-1) + 1

esDivisible :: Int -> Int -> Bool
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

medioFact :: Int -> Int
medioFact 0 = 1
medioFact 1 = 1
medioFact n = n * medioFact(n-2)

cantDigitos :: Integer -> Integer
cantDigitos n   | n < 10 = 1
                        | otherwise = cantDigitos (div n 10) + 1

queDigitoEs :: Integer -> Integer -> Integer
queDigitoEs n k | k == 1 = div (mod n 10) 1
                   | otherwise = div (mod n (10^k)) (10^(k-1))

todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n = queDigitoEs n (cantDigitos n) == queDigitoEs n (cantDigitos n-1)

iEsimoDigito :: Integer -> Integer -> Integer
iEsimoDigito n m = div (mod n (10^(cantDigitos n - m + 1))) (10^(cantDigitos n - m))

sumaDigitos :: Integer -> Integer
sumaDigitos n = (iEsimoDigito n (cantDigitos n)) + (iEsimoDigito n (cantDigitos n - 1))