type Punto2D = (Float, Float)

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

esPar :: Int -> Bool
esPar n = mod n 2 == 0

posPrimerPar :: (Int, Int, Int) -> Int
posPrimerPar (a,b,c) | esPar a = 0
                     | esPar b = 1
                     | esPar c = 2
                     | otherwise = 4

factorial :: Int -> Int
factorial n | n == 0 = 1
            | otherwise = n * factorial (n-1)

fibonacci :: Int -> Int
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | otherwise = fibonacci (n-1) + fibonacci (n-2)

parteEntera :: Float -> Int
parteEntera n | n < 1 = 0
              | otherwise = parteEntera (n-1) + 1
--el +1 suma la cantidad de veces que la funcion parteEntera es llamada, por lo tanto el +1 va a ser llamado n veces.

iesimoDigito :: Int -> Int -> Int
iesimoDigito n i | cantDigitosNo==i = mod n 10
                 | otherwise = iesimoDigito(div n 10) i

cantDigitos :: Int -> Int
cantDigitos n | n < 10 = 1
              | otherwise = cantDigitos(div n 10) + 1