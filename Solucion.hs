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
esMinuscula caracter = ord caracter >= ord 'a' && ord caracter <= ord 'z'

-- EJ 2
letraANatural :: Char -> Int
letraANatural caracter = ord caracter - ord 'a'

-- EJ 3
desplazar :: Char -> Int -> Char
desplazar caracter numero | not (esMinuscula caracter) = caracter 
                          | numero < -26 = desplazar caracter (numero + 26)
                          | numero > 26 = desplazar caracter (numero - 26)
                          | (letraANatural caracter) + numero < 0 = chr ((letraANatural caracter) + numero + ord 'a' + 26) 
                          | (letraANatural caracter) + numero <= 25 = chr ((letraANatural caracter) + numero + ord 'a') 
                          | otherwise = chr ((letraANatural caracter) + numero + ord 'a' - 26) 

-- EJ 4
cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar (caracter:palabraResto) numero = desplazar caracter numero:cifrar palabraResto numero 

-- EJ 5
descifrar :: String -> Int -> String
descifrar palabra numero = cifrar palabra (-numero)

-- EJ 6
cifrarLista :: [String] -> [String]
cifrarLista lista = cifrarPalabrasPosicion lista 0

cifrarPalabrasPosicion :: [String] ->  Int ->  [String]
cifrarPalabrasPosicion [] _ = []
cifrarPalabrasPosicion (palabra:listaResto) posicion = cifrar palabra posicion:cifrarPalabrasPosicion listaResto (posicion + 1)

-- EJ 7
frecuencia :: String -> [Float]
frecuencia palabra = buscaFreq palabra 0

buscaFreq :: String -> Int -> [Float]
buscaFreq palabra 26 = []
buscaFreq palabra numeroletra | cuentaMinusculas palabra == 0 = 0.0:buscaFreq palabra (numeroletra + 1)
                              | otherwise = ((cantidadApariciones (chr (numeroletra + ord 'a')) palabra) * 100) / fromIntegral (cuentaMinusculas palabra):buscaFreq palabra (numeroletra + 1) 

cantidadApariciones :: Char -> String -> Float
cantidadApariciones _ [] = 0 
cantidadApariciones elemento (letra:palabraResto) | elemento == letra = 1 + cantidadApariciones elemento palabraResto
                             | otherwise = cantidadApariciones elemento palabraResto

cuentaMinusculas :: String -> Int
cuentaMinusculas [] = 0
cuentaMinusculas (caracter:palabraResto) | esMinuscula caracter = 1 + cuentaMinusculas palabraResto
                                         | otherwise = cuentaMinusculas palabraResto

-- Ej 8
cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente palabra numero = letraMasFreq (frecuencia (cifrar palabra numero)) 0 

letraMasFreq :: [Float] -> Int -> (Char, Float)
letraMasFreq (frecuencia:frecuenciaResto) numero | frecuencia == maximo (frecuencia:frecuenciaResto) = (chr (numero + ord 'a'), frecuencia)
                                                 | otherwise = letraMasFreq frecuenciaResto (numero + 1)

maximo :: (Num a, Ord a) => [a] -> a
maximo [elemento] = elemento
maximo (elem1:elem2:elementosResto) | elem1 >= elem2 = maximo (elem1:elementosResto)
                                    | otherwise = maximo (elem2:elementosResto)

-- EJ 9
esDescifrado :: String -> String -> Bool
esDescifrado palab1 palab2 = comparaPosiblesCifrados palab1 palab2 0

comparaPosiblesCifrados :: String -> String -> Int -> Bool
comparaPosiblesCifrados _ _ 26 = False
comparaPosiblesCifrados palab1 palab2 numeroPosibleCif = cifrar palab1 numeroPosibleCif == palab2 || comparaPosiblesCifrados palab1 palab2 (numeroPosibleCif + 1) 

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