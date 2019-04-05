
signo numero | numero < 0 = (-1)
             | numero >= 0 = 1

precioDeEntrada edad | edad < 18 = 0
                     | edad < 65 = edad
                     | edad >= 65 = 20

precioDeEntrada edad | edad < 18 = 0
                     | edad < 65 = 60
                     | otherwise = 20

entra edad = edad > 5

esPositivo numero = numero >= 0
cuadrante (x,y) | esPositivo x && esPositivo y = 1
                | esPositivo x && not (esPositivo y) = 2
                | not (esPositivo x) && not (esPositivo y) = 3
                | otherwise = 4


asdf x | x == 'a' = True
       | x == 'b' = False
-- *Main> asdf 'v'
--     *** Exception: asdf.hs:(22,1)-(23,25): Non-exhaustive patterns in function asdf

-- Ejercicios
esVocal unaLetra | unaLetra == 'a' = True
 | unaLetra == 'e' = True
 | unaLetra == 'i' = True
 | unaLetra == 'o' = True
 | unaLetra == 'o' = True
 | otherwise = False -- Funciona, pero es más feo que la versión con pattern matching

modulo unNumero | unNumero >= 0 = unNumero
                | unNumero < 0 = (-unNumero)

funcionLoca unNumero | unNumero < -5 = 1
                    | unNumero <  5 = unNumero
                    | unNumero == 5 = 5
                    | otherwise =  unNumero^2

-- Orden superior
entranTodos [] = True
entranTodos lista = entra (head lista) && entranTodos (tail lista)

entraAlguno [] = False
entraAlguno lista  = entra (head lista) || entraAlguno (tail lista)


pagaMasDe50 edad = precioDeEntrada edad > 50
paganTodosMasDe50 [] = True
paganTodosMasDe50 lista = pagaMasDe50 (head lista) && paganTodosMasDe50 (tail lista)

algunoPagaMasDe50 [] = False
algunoPagaMasDe50 lista  = pagaMasDe50 (head lista) || algunoPagaMasDe50 (tail lista)

alguno condicion [] = False
alguno condicion lista  = condicion (head lista) || alguno condicion (tail lista)

todos condicion [] = True
todos condicion lista  = condicion (head lista) && todos condicion (tail lista)

duplicar palabra = palabra ++ palabra
listaDuplicada palabras = map duplicar palabras

contarLetras palabras = sum(map length palabras)
-- Aplicación parcial
paganTodosMasDe50' lista = all (>50) (map precioDeEntrada lista)
algunoPagaMasDe50' lista = any (>50) (map precioDeEntrada lista)

todasMayusculas palabra = all (flip elem ['A'..'Z']) palabra
palabrasTodasMayusculas palabras = filter todasMayusculas palabras
-- Composición
f x = x + 1
g x = x * 2
h x = (f.g) x

cuadradoPar numeros = filter (even.(^2)) numeros
algunaEmpiezaConVocal palabras = any (esVocal.head) palabras
