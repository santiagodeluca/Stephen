module Solucion where
import Data.Char
-- No se permite agrear nuevos imports
-- Sólo está permitido usar estas funciones:
-- https://campus.exactas.uba.ar/pluginfile.php/557895/mod_resource/content/1/validas_tp.pdf


-- Completar!
-- Nombre de grupo: {Stephen Currificacion}
-- Integrante1: { 45919293,De Luca Santiago Leonel,santidelu510@gmail.com}
-- Integrante2: { 45478235,Aguilar Lautaro,lautaroaguilar.c@gmail.com}
-- Integrante3: { 44006697,Fritzler Nicolas Ezequiel,nicolasfritzler89@gmail.com}
-- Integrante4: { 45073713,Di Bella Valentino,valentdb13@gmail.com}
-- Integrantes que abandonaron la materia: {}

-- EJ 1
esMinuscula :: Char -> Bool
esMinuscula caracter = ord caracter >= ord 'a' && ord caracter <= ord 'z'

-- EJ 2
letraANatural :: Char -> Int
letraANatural caracter = ord caracter - ord 'a'

-- EJ 3
desplazar :: Char -> Int -> Char
desplazar caracter numero | not (esMinuscula caracter) = caracter 
                          | numero <= -26 = desplazar caracter (numero + 26)
                          | numero >= 26 = desplazar caracter (numero - 26)
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
cifrarLista lista = cifrarPalabrasSegunPosicion lista 0

cifrarPalabrasSegunPosicion :: [String] ->  Int ->  [String]
cifrarPalabrasSegunPosicion [] _ = []
cifrarPalabrasSegunPosicion (palabra:listaResto) posicion = cifrar palabra posicion:cifrarPalabrasSegunPosicion listaResto (posicion + 1)

-- EJ 7
frecuencia :: String -> [Float]
frecuencia palabra = buscaFreq palabra 0

-- dada palabra calcula el porcentaje de la frecuencia de cada letra minuscula del abecedario y devuelve todos los valores en una lista
buscaFreq :: String -> Int -> [Float]
buscaFreq palabra 26 = []
buscaFreq palabra numeroLetra | cuentaMinusculas palabra == 0 = 0.0:buscaFreq palabra (numeroLetra + 1)
                              | otherwise = ((cantidadApariciones (chr (numeroLetra + ord 'a')) palabra) * 100) / fromIntegral (cuentaMinusculas palabra):buscaFreq palabra (numeroLetra + 1) 

cantidadApariciones :: Char -> String -> Float
cantidadApariciones _ [] = 0 
cantidadApariciones caracter (letra:palabraResto) | caracter == letra = 1 + cantidadApariciones caracter palabraResto
                                                  | otherwise = cantidadApariciones caracter palabraResto

cuentaMinusculas :: String -> Int
cuentaMinusculas [] = 0
cuentaMinusculas (caracter:palabraResto) | esMinuscula caracter = 1 + cuentaMinusculas palabraResto
                                         | otherwise = cuentaMinusculas palabraResto

-- Ej 8
cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente palabra numero = letraMasFreq (frecuencia (cifrar palabra numero)) 0 

-- dada una lista de frecuencias toma el numero de la posicion en la que se encuentra la frecuencia maxima y devuelve el caracter correspondiente a ese numero junto a su frecuencia
letraMasFreq :: [Float] -> Int -> (Char, Float)
letraMasFreq (frecuencia:frecuenciaResto) posicion | frecuencia == maximo (frecuencia:frecuenciaResto) = (chr (posicion + ord 'a'), frecuencia)
                                                   | otherwise = letraMasFreq frecuenciaResto (posicion + 1)

maximo :: (Num a, Ord a) => [a] -> a
maximo [elemento] = elemento
maximo (elem1:elem2:elementosResto) | elem1 >= elem2 = maximo (elem1:elementosResto)
                                    | otherwise = maximo (elem2:elementosResto)

-- EJ 9
esDescifrado :: String -> String -> Bool
esDescifrado palab1 palab2 = comparaPosiblesCifrados palab1 palab2 0

-- dadas dos palabras prueba con todos los cifrados posibles (26) y devuelve True si en alguno son iguales
comparaPosiblesCifrados :: String -> String -> Int -> Bool
comparaPosiblesCifrados _ _ 26 = False
comparaPosiblesCifrados palab1 palab2 numeroPosibleCif = cifrar palab1 numeroPosibleCif == palab2 || comparaPosiblesCifrados palab1 palab2 (numeroPosibleCif + 1) 

-- EJ 10
todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados [] = []
todosLosDescifrados (palabra:listaResto) = comparaTodosLosCifrados palabra listaResto ++ todosLosDescifrados listaResto 

-- dada una palabra y una lista devuelve cuales son las palabras que son un cifrado de palabra
comparaTodosLosCifrados :: String -> [String] -> [(String, String)]
comparaTodosLosCifrados palabra [] = []
comparaTodosLosCifrados palabra (posibCifrado:listaResto) | esDescifrado palabra posibCifrado = (palabra, posibCifrado):(posibCifrado, palabra):comparaTodosLosCifrados palabra listaResto
                                                          | otherwise = comparaTodosLosCifrados palabra listaResto

-- EJ 11
expandirClave :: String -> Int -> String
expandirClave clave longitudClave | longitudClave > length clave = expandirClave (clave ++ clave) longitudClave 
                                  | longitudClave < length clave = acortarClave clave longitudClave
                                  | otherwise = clave


acortarClave :: String -> Int -> String
acortarClave _ 0 = []
acortarClave (caracter:restoClave) longitudClave = caracter:acortarClave restoClave (longitudClave - 1)

-- EJ 12
cifrarVigenere :: String -> String -> String
cifrarVigenere palabra clave = desplazaVigenere palabra (expandirClave clave (length palabra))

-- dada una palabra y una clave desplaza cada caracter de palabra por el valor de letraANatural de clave en ese caracter
desplazaVigenere :: String -> String -> String
desplazaVigenere [] _ = []
desplazaVigenere (caracter:restoPalabra) (caracterClave:restoClave) = desplazar caracter (letraANatural caracterClave):desplazaVigenere restoPalabra restoClave

-- EJ 13
descifrarVigenere :: String -> String -> String
descifrarVigenere palabra clave = cifrarVigenere palabra (invertir clave)

-- genera la clave "opuesta", es decir, la clave que al cifrar un cifrado vigenere devuelve la palabra original. la 'a' es la unica letra que se mantiene simultaneamente en una clave y su opuesta.
invertir :: String -> String
invertir [] = []
invertir (caracter:restoClave) | caracter == 'a' = 'a':invertir restoClave
                               | otherwise = chr (letraANatural 'z' - letraANatural caracter + ord 'a' + 1):invertir restoClave

-- EJ 14
peorCifrado :: String -> [String] -> String
peorCifrado _ [clave] = clave 
peorCifrado palabra (clave1:clave2:restoClave) | distanciaSecuencias (cifrarVigenere palabra clave1) palabra >= distanciaSecuencias (cifrarVigenere palabra clave2) palabra = peorCifrado palabra (clave2:restoClave)
                                               | otherwise = peorCifrado palabra (clave1:restoClave)

distanciaSecuencias :: String -> String -> Int
distanciaSecuencias [] _ = 0
distanciaSecuencias (caracter1:restoPalabra1) (caracter2:restoPalabra2) = absoluto (letraANatural caracter1 - letraANatural caracter2) + distanciaSecuencias restoPalabra1 restoPalabra2

absoluto :: Int -> Int 
absoluto numero | numero < 0 = (-numero)
                | otherwise = numero

-- EJ 15
combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]
combinacionesVigenere [] _ _ = []
combinacionesVigenere (palabra:restoLista) claves cifrados = encuentraCombinacion palabra claves cifrados ++ combinacionesVigenere restoLista claves cifrados

-- encuentra todas las claves que hacen que cifrar palabra de el cifrado buscado
encuentraCombinacion :: String -> [String] -> String -> [(String, String)]
encuentraCombinacion _ [] _ = []
encuentraCombinacion palabra (clave:restoClaves) cifrado | cifrarVigenere palabra clave == cifrado = [(palabra, clave)] ++ encuentraCombinacion palabra restoClaves cifrado
                                                         | otherwise = encuentraCombinacion palabra restoClaves cifrado 