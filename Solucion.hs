module Solucion where
import Data.Char
-- No se permite agrear nuevos imports
-- Sólo está permitido usar estas funciones:
-- https://campus.exactas.uba.ar/pluginfile.php/557895/mod_resource/content/1/validas_tp.pdf


-- Completar!
-- Nombre de grupo: {Stephen Currificacion}
-- Integrante1: { 45919293,De Luca Santiago Leonel}
-- Integrante2: { 45478235,Aguilar Lautaro}
-- Integrante3: { 44006697,Fritzler Nicolas Ezequiel}
-- Integrante4: { 45073713,Di Bella Valentino}
-- Integrantes que abandonaron la materia: {En caso que haya abandonado la materia algún
                        -- integrante, completar con los dni y apellidos, sino dejar vacío}

-- EJ 1
esMinuscula :: Char -> Bool
esMinuscula a = ord a >= ord 'a' && ord a <= ord 'z'

-- EJ 2
letraANatural :: Char -> Int
letraANatural a = (ord a) - (ord 'a')

-- EJ 3
desplazar :: Char -> Int -> Char
desplazar a n | not (esMinuscula a) = a 
              | (letraANatural a) + n < 0 = chr ((letraANatural a) + n + ord 'a' + 26) 
              | (letraANatural a) + n <= 25 = chr ((letraANatural a) + n + ord 'a') 
              | otherwise = chr ((letraANatural a) + n + ord 'a' - 26) 

-- EJ 4
cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar (x:xs) n | esMinuscula x = desplazar x n:cifrar xs n 
                | otherwise = x:cifrar xs n 

-- EJ 5
descifrar :: String -> Int -> String
descifrar x n = cifrar x (-n)

-- EJ 6
cifrarLista :: [String] -> [String]
cifrarLista x = cifrarPalabrasPosicion x x

cifrarPalabrasPosicion :: [String] ->  [String] ->  [String]
cifrarPalabrasPosicion [] _ = []
cifrarPalabrasPosicion (x:xs) y = cifrar x (length y - length (x:xs)):cifrarPalabrasPosicion xs y

-- EJ 7
frecuencia :: String -> [Float]
frecuencia x = buscaFreq x 0

buscaFreq :: String -> Int -> [Float]
buscaFreq x 26 = []
buscaFreq x n | cuentaMinusculas x == 0 = 0.0:buscaFreq x (n + 1)
              | otherwise = ((cantidadApariciones (chr (n + ord 'a')) x) * 100) / fromIntegral (cuentaMinusculas x):buscaFreq x (n + 1) 

cantidadApariciones :: Char -> String -> Float
cantidadApariciones _ [] = 0 
cantidadApariciones e (x:xs) | e == x = 1 + cantidadApariciones e xs
                             | otherwise = cantidadApariciones e xs

cuentaMinusculas :: String -> Int
cuentaMinusculas [] = 0
cuentaMinusculas (x:xs) | esMinuscula x = 1 + cuentaMinusculas xs
                        | otherwise = cuentaMinusculas xs

-- Ej 8
cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente x n = letraMasFreq (frecuencia (cifrar x n)) 0 

letraMasFreq :: [Float] -> Int -> (Char, Float)
letraMasFreq (x:xs) n | x == maximo (x:xs) = (chr (n + ord 'a'), x)
                      | otherwise = letraMasFreq xs (n + 1)

maximo :: (Num a, Ord a) => [a] -> a
maximo [x] = x
maximo (x:y:xs) | x >= y = maximo (x:xs)
                | otherwise = maximo (y:xs)

-- EJ 9
esDescifrado :: String -> String -> Bool
esDescifrado x y = comparaPosiblesCifrados x y 0

comparaPosiblesCifrados :: String -> String -> Int -> Bool
comparaPosiblesCifrados _ _ 26 = False
comparaPosiblesCifrados x y n = cifrar x n == y || comparaPosiblesCifrados x y (n + 1) 

-- EJ 10
todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados [] = []
todosLosDescifrados (x:xs) = comparaTodosLosCifrados x xs ++ todosLosDescifrados xs 

comparaTodosLosCifrados :: String -> [String] -> [(String, String)]
comparaTodosLosCifrados x [] = []
comparaTodosLosCifrados x (y:ys) | esDescifrado x y = (x, y):(y, x):comparaTodosLosCifrados x ys
                                 | otherwise = comparaTodosLosCifrados x ys

-- EJ 11

expandirClave :: String -> Int -> String
expandirClave x n | n > length x = expandirClave (x ++ x) n 
                  | n < length x = acortarClave x n
                  | otherwise = x


acortarClave :: String -> Int -> String
acortarClave (x:xs) 0 = []
acortarClave (x:xs) n = x:acortarClave xs (n - 1)

-- EJ 12
cifrarVigenere :: String -> String -> String
cifrarVigenere p c = desplazaVigenere p (expandirClave c (length p))

desplazaVigenere :: String -> String -> String
desplazaVigenere [] _ = []
desplazaVigenere (x:xs) (y:ys) = desplazar x (letraANatural y):desplazaVigenere xs ys

-- EJ 13
descifrarVigenere :: String -> String -> String
descifrarVigenere p c = cifrarVigenere p (invertir c)

invertir :: String -> String
invertir [] = []
invertir (x:xs) = chr (letraANatural 'z' - letraANatural x + ord 'a' + 1):invertir xs

-- EJ 14
peorCifrado :: String -> [String] -> String
peorCifrado _ [x] = x 
peorCifrado p (x:y:xs) | distanciaSecuencias (cifrarVigenere p x) p >= distanciaSecuencias (cifrarVigenere p y) p = peorCifrado p (y:xs)
                       | otherwise = peorCifrado p (x:xs)

distanciaSecuencias :: String -> String -> Int
distanciaSecuencias [] _ = 0
distanciaSecuencias (x:xs) (y:ys) = absoluto (letraANatural x - letraANatural y) + distanciaSecuencias xs ys

absoluto :: Int -> Int 
absoluto x | x < 0 = (-x)
           | otherwise = x

-- EJ 15
combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]
combinacionesVigenere [] _ _ = []
combinacionesVigenere (x:xs) cl ci = encuentraCombinacion x cl ci ++ combinacionesVigenere xs cl ci

encuentraCombinacion :: String -> [String] -> String -> [(String, String)]
encuentraCombinacion _ [] _ = []
encuentraCombinacion m (x:xs) c | cifrarVigenere m x == c = [(m, x)] ++ encuentraCombinacion m xs c
                                | otherwise = encuentraCombinacion m xs c 