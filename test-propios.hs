import Test.HUnit
import Solucion
import Data.List
-- No está permitido agregar nuevos imports.

runPropiosTests = runTestTT allTests

allTests = test [
    "esMinuscula" ~: testsEjesMinuscula,
    "letraANatural" ~: testsEjletraANatural,
    "desplazar" ~: testsEjdesplazar,
    "cifrar" ~: testsEjcifrar,
    "descifrar" ~: testsEjdescifrar,
    "cifrarLista" ~: testsEjcifrarLista,
    "frecuencia" ~: testsEjfrecuencia,
    "cifradoMasFrecuente" ~: testsEjcifradoMasFrecuente,
    "esDescifrado" ~: testsEjesDescifrado,
    "todosLosDescifrados" ~: testsEjtodosLosDescifrados,
    "expandirClave" ~: testsEjexpandirClave,
    "cifrarVigenere" ~: testsEjcifrarVigenere,
    "descifrarVigenere" ~: testsEjdescifrarVigenere,
    "peorCifrado" ~: testsEjpeorCifrado,
    "combinacionesVigenere" ~: testsEjcombinacionesVigenere
    ]


testsEjesMinuscula = test [
    "generico" ~: esMinuscula 'd' ~?= True,
    "primera minuscula" ~: esMinuscula 'a' ~?= True,
    "ultima minuscula" ~: esMinuscula 'z' ~?= True,
    "mayuscula 1" ~: esMinuscula 'T' ~?= False,
    "mayuscula 2" ~: esMinuscula 'A' ~?= False,
    "mayuscula 3" ~: esMinuscula 'Z' ~?= False,
    "numero" ~: esMinuscula '8' ~?= False,
    "no alfanumerico" ~: esMinuscula '!' ~?= False
    ]

testsEjletraANatural = test [
    "generico" ~: letraANatural 'b' ~?= 1,
    "primera letra" ~: letraANatural 'a' ~?= 0,
    "ultima letra" ~: letraANatural 'z' ~?= 25
    ]

testsEjdesplazar = test [
    "generico adelante" ~: desplazar 'a' 3 ~?= 'd',
    "generico atras" ~: desplazar 'a' (-3) ~?= 'x',
    "se pasa" ~: desplazar 'z' 2 ~?= 'b',
    "deja igual sin mover" ~: desplazar 'a' 0 ~?= 'a',
    "deja igual toda la vuelta adelante" ~: desplazar 'a' 26 ~?= 'a',
    "deja igual da dos vueltas adelante" ~: desplazar 'a' 52 ~?= 'a',
    "deja igual da dos vueltas para atras" ~: desplazar 'a' (-52) ~?= 'a',
    "deja igual toda la vuelta para atras" ~: desplazar 'a' (-26) ~?= 'a',
    "no desplaza porque es mayus" ~: desplazar 'A' 2 ~?= 'A',
    "no desplaza porque es numero" ~: desplazar '3' 2 ~?= '3',
    "no desplaza porque es simbolo" ~: desplazar '!' 2 ~?= '!',
    "no desplaza porque es espacio" ~: desplazar ' ' 2 ~?= ' '
    ]

testsEjcifrar = test [
    "generico" ~: cifrar "computacion" 3 ~?= "frpsxwdflrq",
    "con algunas mayus" ~: cifrar "computaciON" 3 ~?= "frpsxwdflON",
    "con algunos numeros" ~: cifrar "computaci12" 3 ~?= "frpsxwdfl12",
    "frase con espacios" ~: cifrar "introduccion a la programacion" 3 ~?= "lqwurgxfflrq d od surjudpdflrq",
    "abecedario" ~: cifrar "abcdefghijklmnopqrstuvwxyz" 1 ~?= "bcdefghijklmnopqrstuvwxyza",
    "deja igual" ~: cifrar "computacion" 0 ~?= "computacion",
    "deja igual da la vuelta" ~: cifrar "computacion" 26 ~?= "computacion",
    "deja igual da dos vueltas" ~: cifrar "computacion" 52 ~?= "computacion",
    "da la vuelta y mueve uno" ~: cifrar "aaa" 27 ~?= "bbb",
    "vacio" ~: cifrar "" 7 ~?= ""
    ]

testsEjdescifrar = test [
    "generico" ~: descifrar "frpsxwdflrq" 3 ~?= "computacion",
    "con algunas mayus" ~: descifrar "frpsxwdflON" 3 ~?= "computaciON",
    "con algunos numeros" ~: descifrar "frpsxwdfl12" 3 ~?= "computaci12",
    "frase con espacios" ~: descifrar "lqwurgxfflrq d od surjudpdflrq" 3 ~?= "introduccion a la programacion",
    "abecedario" ~: descifrar "bcdefghijklmnopqrstuvwxyza" 1 ~?= "abcdefghijklmnopqrstuvwxyz",
    "deja igual" ~: descifrar "computacion" 0 ~?= "computacion",
    "deja igual da la vuelta" ~: descifrar "computacion" 26 ~?= "computacion",
    "deja igual da dos vueltas" ~: descifrar "computacion" 52 ~?= "computacion",
    "da la vuelta y mueve uno" ~: descifrar "bbb" 27 ~?= "aaa",
    "vacio" ~: descifrar "" 7 ~?= ""
    ]

testsEjcifrarLista = test [
    "generico" ~: cifrarLista ["compu", "labo", "intro"] ~?= ["compu", "mbcp", "kpvtq"],
    "vacio" ~: cifrarLista [] ~?= [],
    "unico elemento vacio" ~: cifrarLista [""] ~?= [""],
    "unica palabra" ~: cifrarLista ["compu"] ~?= ["compu"],
    "todos a uno de dist deberian ser iguales" ~: cifrarLista ["aaa", "zzz", "yyy"] ~?= ["aaa", "aaa", "aaa"],
    "numeros" ~: cifrarLista ["aaa1", "zzz2", "yyy3"] ~?= ["aaa1", "aaa2", "aaa3"],
    "numeros y mayusculas" ~: cifrarLista ["Aaaa1", "Zzzz2", "Yyyy3"] ~?= ["Aaaa1", "Zaaa2", "Yaaa3"]
    ]

testsEjfrecuencia = test [
    "generico" ~: expectlistProximity (frecuencia "taller") [16.666668,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0,33.333336,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0],
    "unica letra es la primera" ~: expectlistProximity (frecuencia "a") [100.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    "unica letra es la ultima" ~: expectlistProximity (frecuencia "z") [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,100.0],
    "abecedario(todas las letras igual frecuencia)" ~: expectlistProximity (frecuencia "abcdefghijklmnopqrstuvwxyz") [3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537],
    "mayusculas" ~: expectlistProximity (frecuencia "HOLA") [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    "mayusculas y minus" ~: expectlistProximity (frecuencia "HOLAaa") [100.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    "sin letras" ~: expectlistProximity (frecuencia "!465^%$") [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    "vacio" ~: expectlistProximity (frecuencia "") [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
    ]

testsEjcifradoMasFrecuente = test [
    "generico" ~: expectAnyTuplaAprox (cifradoMasFrecuente "taller" 3) [('o', 33.333336)],
    "unica letra es la primera" ~: expectAnyTuplaAprox (cifradoMasFrecuente "a" 1) [('b', 100.0)],
    "unica letra es la ultima" ~: expectAnyTuplaAprox (cifradoMasFrecuente "z" 1) [('a', 100.0)],
    "generico 50%" ~: expectAnyTuplaAprox (cifradoMasFrecuente "casa" 2) [('c', 50.0)],
    "generico 50% para atras" ~: expectAnyTuplaAprox (cifradoMasFrecuente "casa" (-2)) [('y', 50.0)],
    "empate" ~: expectAnyTuplaAprox (cifradoMasFrecuente "aabb" 1) [('b', 50.0), ('c', 50.0)],
    "deja igual" ~: expectAnyTuplaAprox (cifradoMasFrecuente "afk" 0) [('a', 33.333333), ('f', 33.333333), ('k', 33.333333)],
    "mayus, numeros y una minus" ~: expectAnyTuplaAprox (cifradoMasFrecuente "HOLAa432" 2) [('c', 100.0)]
    ]

testsEjesDescifrado = test [
    "generico" ~: esDescifrado "taller" "compu" ~?= False,
    "mayus" ~: esDescifrado "AA" "aa" ~?= False,
    "numeros" ~: esDescifrado "45" "aa" ~?= False,
    "numeros y letras" ~: esDescifrado "aa45" "aa" ~?= False,
    "primeras coinciden pero longitudes son dist" ~: esDescifrado "hola" "holanda" ~?= False,
    "misma palabra" ~: esDescifrado "aaa" "aaa" ~?= True,
    "palabra diferente es descifrado" ~: esDescifrado "aaa" "zzz" ~?= True,
    "palabra diferente es descifrado para atras" ~: esDescifrado "zzz" "aaa" ~?= True
    ]

testsEjtodosLosDescifrados = test [
    "generico" ~: expectPermutacion (todosLosDescifrados ["compu", "frpsx", "mywza"]) [("compu", "frpsx"), ("frpsx", "compu")],
    "con numeros" ~: expectPermutacion (todosLosDescifrados ["14t", "14r"]) [("14r", "14t"), ("14t", "14r")],
    "ninguno es descifrado" ~: expectPermutacion (todosLosDescifrados ["aaa", "cbc"]) [],
    "palabra y vacio" ~: expectPermutacion (todosLosDescifrados ["aaa", ""]) [],
    "unica palabra" ~: expectPermutacion (todosLosDescifrados ["aaa"]) [],
    "unica palabra vacia" ~: expectPermutacion (todosLosDescifrados [""]) [],
    "vacio" ~: expectPermutacion (todosLosDescifrados []) []
    ]

testsEjexpandirClave = test [
    "generico" ~: expandirClave "compu" 8 ~?= "compucom",
    "reduce" ~: expandirClave "compu" 2 ~?= "co",
    "reduce a uno" ~: expandirClave "compu" 1 ~?= "c",
    "duplica" ~: expandirClave "compu" 10 ~?= "compucompu",
    "extiende una sola letra" ~: expandirClave "c" 8 ~?= "cccccccc",
    "deja igual" ~: expandirClave "compu" 5 ~?= "compu"
    ]

testsEjcifrarVigenere = test [
    "generico" ~: cifrarVigenere "computacion" "ip" ~?= "kdueciirqdv",
    "generico 2" ~: cifrarVigenere "aaa" "b" ~?= "bbb",
    "deja igual, unica 'a'" ~: cifrarVigenere "computacion" "a" ~?= "computacion",
    "deja igual, mas de una 'a'" ~: cifrarVigenere "computacion" "aaaaa" ~?= "computacion",
    "da la vuelta" ~: cifrarVigenere "xyz" "abcabc" ~?= "xzb",
    "numeros y mayus" ~: cifrarVigenere "xyz8547HOLA" "abcabc" ~?= "xzb8547HOLA",
    "espacios" ~: cifrarVigenere "xyz 85x" "abcabc" ~?= "xzb 85x",
    "vacio" ~: cifrarVigenere "" "abcabc" ~?= ""
    ]

testsEjdescifrarVigenere = test [
    "generico" ~: descifrarVigenere "kdueciirqdv" "ip" ~?= "computacion",
    "generico 2" ~: descifrarVigenere "bbb" "b" ~?= "aaa",
    "deja igual, unica 'a'" ~: descifrarVigenere "computacion" "a" ~?= "computacion",
    "deja igual, mas de una 'a'" ~: descifrarVigenere "computacion" "aaaaa" ~?= "computacion",
    "da la vuelta" ~: descifrarVigenere "xzb" "abcabc" ~?= "xyz",
    "numeros y mayus" ~: descifrarVigenere "xzb8547HOLA" "abcabc" ~?= "xyz8547HOLA",
    "espacios" ~: descifrarVigenere "xzb 85x" "abcabc" ~?= "xyz 85x",
    "vacio" ~: descifrarVigenere "" "abcabc" ~?= ""
    ]

testsEjpeorCifrado = test [
    "generico" ~: peorCifrado "computacion" ["ip", "asdef", "ksy"] ~?= "asdef",
    "'a' siempre es peor" ~: peorCifrado "computacion" ["aaa", "bbb", "ccc", "zzz"] ~?= "aaa",
    "da la vuelta" ~: peorCifrado "zzzzzzzzzzz" ["aaa", "bbb", "ccc"] ~?= "aaa",
    "diferencia entre palabra mas larga que claves (parte 1)" ~: peorCifrado "computacion" ["aaaazzzz", "bbbbaaaa"] ~?= "bbbbaaaa",
    "diferencia entre palabra mas corta que claves (parte 2, mismas claves que antes pero resultado diferente)" ~: peorCifrado "casa" ["aaaazzzz", "bbbbaaaa"] ~?= "aaaazzzz",
    "unica clave" ~: peorCifrado "computacion" ["aaa"] ~?= "aaa",
    "vacio, hay empate" ~: expectAny (peorCifrado "" ["ccc", "aaa"]) ["aaa", "ccc"]
    ]

testsEjcombinacionesVigenere = test [
    "generico" ~: expectPermutacion (combinacionesVigenere ["hola", "mundo"] ["a", "b"] "ipmb") [("hola", "b")],
    "clave deja igual algunos" ~: expectPermutacion (combinacionesVigenere ["aaa", "bbb"] ["a", "b"] "bbb") [("aaa", "b"), ("bbb", "a")],
    "da la vuelta" ~: expectPermutacion (combinacionesVigenere ["zaaz", "lool"] ["a", "b"] "abba") [("zaaz", "b")],
    "con mayus" ~: expectPermutacion (combinacionesVigenere ["axtm", "papo"] ["flaco", "FLACO", "charly"] "fito") [("axtm", "flaco")],
    "con numeros" ~: expectPermutacion (combinacionesVigenere ["hola12", "mundo"] ["a", "b"] "ipmb12") [("hola12", "b")],
    "con espacios" ~: expectPermutacion (combinacionesVigenere ["uah trucxth", "queen"] ["zeppelin", "rolling"] "los beatles") [("uah trucxth", "rolling")],
    "vacio" ~: expectPermutacion (combinacionesVigenere [] [] "bbb") [],
    "unica palabra vacia" ~: expectPermutacion (combinacionesVigenere [""] ["aaa"] "") [("", "aaa")],
    "palabra vacia no coincide" ~: expectPermutacion (combinacionesVigenere [""] ["aaa"] "no") [],
    "no coincide devuelve vacio" ~: expectPermutacion (combinacionesVigenere ["aaa"] ["zz"] "bbb") []
    ]

-- Funciones útiles

-- margetFloat(): Float
-- asegura: res es igual a 3.84615370001
margenFloat = 0.00001

-- expectAny (actual: a, expected: [a]): Test
-- asegura: res es un Test Verdadero si y sólo si actual pertenece a la lista expected
expectAny :: (Foldable t, Eq a, Show a, Show (t a)) => a -> t a -> Test
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)


-- expectlistProximity (actual: [Float], expected: [Float]): Test
-- asegura: res es un Test Verdadero si y sólo si:
--                  |actual| = |expected|
--                  para todo i entero tal que 0<=i<|actual|, |actual[i] - expected[i]| < margenFloat()
expectlistProximity:: [Float] -> [Float] -> Test
expectlistProximity actual expected = esParecidoLista actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esParecidoLista :: [Float] -> [Float] -> Bool
esParecidoLista actual expected = (length actual) == (length expected) && (esParecidoUnaAUno actual expected)

esParecidoUnaAUno :: [Float] -> [Float] -> Bool
esParecidoUnaAUno [] [] = True
esParecidoUnaAUno (x:xs) (y:ys) = (aproximado x y) && (esParecidoUnaAUno xs ys)

aproximado :: Float -> Float -> Bool
aproximado x y = abs (x - y) < margenFloat


-- expectAnyTuplaAprox (actual: CharxFloat, expected: [CharxFloat]): Test
-- asegura: res un Test Verdadero si y sólo si:
--                  para algun i entero tal que 0<=i<|expected|,
--                         (fst expected[i]) == (fst actual) && |(snd expected[i]) - (snd actual)| < margenFloat()

expectAnyTuplaAprox :: (Char, Float) -> [(Char, Float)] -> Test
expectAnyTuplaAprox actual expected = elemAproxTupla actual expected ~? ("expected any of: " ++ show expected ++ "\nbut got: " ++ show actual)

elemAproxTupla :: (Char, Float) -> [(Char, Float)] -> Bool
elemAproxTupla _ [] = False
elemAproxTupla (ac,af) ((bc,bf):bs) = sonAprox || (elemAproxTupla (ac,af) bs)
    where sonAprox = (ac == bc) && (aproximado af bf)



-- expectPermutacion (actual: [T], expected[T]) : Test
-- asegura: res es un Test Verdadero si y sólo si:
--            para todo elemento e de tipo T, #Apariciones(actual, e) = #Apariciones(expected, e)

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)