import Test.HUnit
import Solucion
import Data.List
-- No está permitido agregar nuevos imports.

runCatedraTests = runTestTT allTests

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
    esMinuscula 'd' ~?= True,
    esMinuscula 'a' ~?= True,
    esMinuscula 'z' ~?= True,
    esMinuscula 'T' ~?= False,
    esMinuscula 'A' ~?= False,
    esMinuscula 'Z' ~?= False,
    esMinuscula '!' ~?= False,
    esMinuscula '8' ~?= False
    ]

testsEjletraANatural = test [
    letraANatural 'b' ~?= 1,
    letraANatural 'a' ~?= 0,
    letraANatural 'z' ~?= 25
    ]

testsEjdesplazar = test [
    desplazar 'a' 3 ~?= 'd',
    desplazar 'a' (-3) ~?= 'x',
    desplazar 'b' 3 ~?= 'e',
    desplazar 'z' 2 ~?= 'b',
    desplazar 'a' 0 ~?= 'a',
    desplazar 'a' 26 ~?= 'a',
    desplazar 'a' 52 ~?= 'a',
    desplazar 'a' (-52) ~?= 'a',
    desplazar 'a' (-26) ~?= 'a',
    desplazar 'A' 2 ~?= 'A',
    desplazar '!' 2 ~?= '!',
    desplazar ' ' 2 ~?= ' '
    ]

testsEjcifrar = test [
    cifrar "computacion" 3 ~?= "frpsxwdflrq",
    cifrar "computaciON" 3 ~?= "frpsxwdflON",
    cifrar "computaci12" 3 ~?= "frpsxwdfl12",
    cifrar "introduccion a la programacion" 3 ~?= "lqwurgxfflrq d od surjudpdflrq",
    cifrar "abcdefghijklmnopqrstuvwxyz" 1 ~?= "bcdefghijklmnopqrstuvwxyza",
    cifrar "computacion" 0 ~?= "computacion",
    cifrar "computacion" 26 ~?= "computacion",
    cifrar "computacion" 52 ~?= "computacion",
    cifrar "aaa" 27 ~?= "bbb",
    cifrar "" 7 ~?= ""
    ]

testsEjdescifrar = test [
    descifrar "frpsxwdflrq" 3 ~?= "computacion",
    descifrar "frpsxwdflON" 3 ~?= "computaciON",
    descifrar "frpsxwdfl12" 3 ~?= "computaci12",
    descifrar "lqwurgxfflrq d od surjudpdflrq" 3 ~?= "introduccion a la programacion",
    descifrar "bcdefghijklmnopqrstuvwxyza" 1 ~?= "abcdefghijklmnopqrstuvwxyz",
    descifrar "computacion" 0 ~?= "computacion",
    descifrar "computacion" 26 ~?= "computacion",
    descifrar "computacion" 52 ~?= "computacion",
    descifrar "bbb" 27 ~?= "aaa",
    descifrar "" 7 ~?= ""
    ]

testsEjcifrarLista = test [
    cifrarLista ["compu", "labo", "intro"] ~?= ["compu", "mbcp", "kpvtq"],
    cifrarLista [] ~?= [],
    cifrarLista [""] ~?= [""],
    cifrarLista ["compu"] ~?= ["compu"],
    cifrarLista ["aaa", "zzz", "yyy"] ~?= ["aaa", "aaa", "aaa"],
    cifrarLista ["aaa1", "zzz2", "yyy3"] ~?= ["aaa1", "aaa2", "aaa3"],
    cifrarLista ["Aaaa1", "Zzzz2", "Yyyy3"] ~?= ["Aaaa1", "Zaaa2", "Yaaa3"]
    ]

testsEjfrecuencia = test [
    expectlistProximity (frecuencia "taller") [16.666668,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0,33.333336,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0],
    expectlistProximity (frecuencia "a") [100.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    expectlistProximity (frecuencia "z") [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,100.0],
    expectlistProximity (frecuencia "abc") [33.333333,33.333333,33.333333,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    expectlistProximity (frecuencia "abcdefghijklmnopqrstuvwxyz") [3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537,3.8461537],
    expectlistProximity (frecuencia "HOLA") [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    expectlistProximity (frecuencia "!465^%$") [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
    ]

testsEjcifradoMasFrecuente = test [
    expectAnyTuplaAprox (cifradoMasFrecuente "taller" 3) [('o', 33.333336)],
    expectAnyTuplaAprox (cifradoMasFrecuente "a" 1) [('b', 100.0)],
    expectAnyTuplaAprox (cifradoMasFrecuente "z" 1) [('a', 100.0)],
    expectAnyTuplaAprox (cifradoMasFrecuente "casa" 2) [('c', 50.0)],
    expectAnyTuplaAprox (cifradoMasFrecuente "casa" (-2)) [('y', 50.0)],
    expectAnyTuplaAprox (cifradoMasFrecuente "aabb" 1) [('b', 50.0), ('c', 50.0)],
    expectAnyTuplaAprox (cifradoMasFrecuente "afk" 0) [('a', 33.333333), ('f', 33.333333), ('k', 33.333333)],
    expectAnyTuplaAprox (cifradoMasFrecuente "HOLAa432" 2) [('c', 100.0)]
    ]

testsEjesDescifrado = test [
    esDescifrado "taller" "compu" ~?= False,
    esDescifrado "AA" "aa" ~?= False,
    esDescifrado "45" "aa" ~?= False,
    esDescifrado "aa45" "aa" ~?= False,
    esDescifrado "hola" "holanda" ~?= False,
    esDescifrado "aaa" "aaa" ~?= True,
    esDescifrado "aaa" "zzz" ~?= True,
    esDescifrado "zzz" "aaa" ~?= True,
    esDescifrado "abc" "xyz" ~?= True
    ]

testsEjtodosLosDescifrados = test [
    todosLosDescifrados ["compu", "frpsx", "mywza"] ~?= [("compu", "frpsx"), ("frpsx", "compu")],
    expectPermutacion (todosLosDescifrados ["aaa", "abc", "zzz", "xyz"]) [("aaa", "zzz"), ("zzz", "aaa"), ("xyz", "abc"), ("abc", "xyz")],
    expectPermutacion (todosLosDescifrados ["14t", "14r"]) [("14r", "14t"), ("14t", "14r")],
    expectPermutacion (todosLosDescifrados ["aaa", "cbc"]) [],
    expectPermutacion (todosLosDescifrados ["aaa", ""]) [],
    expectPermutacion (todosLosDescifrados ["aaa"]) [],
    expectPermutacion (todosLosDescifrados [""]) [],
    expectPermutacion (todosLosDescifrados []) []
    ]

testsEjexpandirClave = test [
    expandirClave "compu" 8 ~?= "compucom",
    expandirClave "compu" 2 ~?= "co",
    expandirClave "compu" 1 ~?= "c",
    expandirClave "compu" 10 ~?= "compucompu",
    expandirClave "c" 8 ~?= "cccccccc"
    ]

testsEjcifrarVigenere = test [
    cifrarVigenere "computacion" "ip" ~?= "kdueciirqdv",
    cifrarVigenere "computacion" "a" ~?= "computacion",
    cifrarVigenere "computacion" "aaaaa" ~?= "computacion",
    cifrarVigenere "aaa" "b" ~?= "bbb",
    cifrarVigenere "aaaaaa" "abc" ~?= "abcabc",
    cifrarVigenere "xyz" "abcabc" ~?= "xzb",
    cifrarVigenere "" "abcabc" ~?= ""
    ]

testsEjdescifrarVigenere = test [
    descifrarVigenere "kdueciirqdv" "ip" ~?= "computacion",
    descifrarVigenere "computacion" "a" ~?= "computacion",
    descifrarVigenere "computacion" "aaaaa" ~?= "computacion",
    descifrarVigenere "bbb" "b" ~?= "aaa",
    descifrarVigenere "abcabc" "abc" ~?= "aaaaaa",
    descifrarVigenere "xzb" "abcabc" ~?= "xyz",
    descifrarVigenere "" "abcabc" ~?= ""
    ]

testsEjpeorCifrado = test [
    peorCifrado "computacion" ["ip", "asdef", "ksy"] ~?= "asdef",
    peorCifrado "computacion" ["aaa", "bbb", "ccc"] ~?= "aaa",
    peorCifrado "zzzzzzzzzzz" ["aaa", "bbb", "ccc"] ~?= "aaa",
    peorCifrado "computacion" ["aaaazzzz", "bbbbaaaa"] ~?= "bbbbaaaa",
    peorCifrado "casa" ["aaaazzzz", "bbbbaaaa"] ~?= "aaaazzzz",
    peorCifrado "computacion" ["aaa"] ~?= "aaa",
    peorCifrado "aaaa" ["abcd", "abce"] ~?= "abcd",
    expectAny (peorCifrado "" ["ccc", "aaa"]) ["aaa", "ccc"]
    ]

testsEjcombinacionesVigenere = test [
    combinacionesVigenere ["hola", "mundo"] ["a", "b"] "ipmb" ~?= [("hola", "b")],
    expectPermutacion (combinacionesVigenere ["aaa", "bbb"] ["a", "b"] "bbb") [("aaa", "b"), ("bbb", "a")],
    expectPermutacion (combinacionesVigenere ["zaaz", "lool"] ["a", "b"] "abba") [("zaaz", "b")],
    expectPermutacion (combinacionesVigenere ["axtm", "papo"] ["flaco", "charly"] "fito") [("axtm", "flaco")],
    expectPermutacion (combinacionesVigenere ["uah trucxth", "queen"] ["zeppelin", "rolling"] "los beatles") [("uah trucxth", "rolling")],
    expectPermutacion (combinacionesVigenere [] [] "bbb") [],
    expectPermutacion (combinacionesVigenere [""] ["aaa"] "") [("", "aaa")],
    expectPermutacion (combinacionesVigenere [""] ["aaa"] "no") [],
    expectPermutacion (combinacionesVigenere ["aaa"] ["zz"] "bbb") []
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