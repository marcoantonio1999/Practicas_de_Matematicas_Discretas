module Practica3 where

--Estructuras Discretas 2018-1
--Profesor: Laura Freidberg Gojman
--Ayudante: Ricardo Jiménez Méndez
--Práctica 3
--Alumno: Marco Antonio Orduña Avila
--No. de Cuenta:315019928

--Recibe un entero x y una lista de enteros
--Elimina el primer entero que sea igual a x
eliminaUno :: Int -> [Int] -> [Int]
eliminaUno n [] = []
eliminaUno n (x:xs)= if (n == x)
                     then [] ++ (xs)					
                     else [x] ++ (eliminaUno n (xs))
--Recibe un elemento a y una lista del mismo tipo que ese elemento
--Elimina todos aquellos elementos que sean iguales a 
eliminaTodos :: (Num a,Eq a) => a -> [a] -> [a]
eliminaTodos n [] = []
eliminaTodos n (x:xs) = if (n == x)
						then [] ++ (eliminaTodos n xs)
						else [x] ++ (eliminaTodos n xs)

--Recibe un entero x y una lista de enteros
--Nos regresa una lista sin el numero de enteros que le indicamos  
tira :: Int -> [Int] -> [Int]
tira k [] = error "error"
tira k (x:xs) = if (k > 1) 
	then []++(tira (k-1) (xs))
	else if (k < 0)
	    then error "error"
		else (xs)
					

--Dada una lista nos regresa la lista volteada
reversa :: [a] -> [a]
reversa [] = error "error"
reversa [x] = [x]
reversa (x:xs) = (reversa xs) ++ [x]

--Dado un entero, hacemos la suma de los cuadrados de ese entero
--Ejemplo: sumaCuad 3 = 9 + 4 + 1 + 0 = 14
sumaCuad :: Int -> Int
sumaCuad n = if (n > 0)
			 then ((n*n) + sumaCuad (n-1))
             else n
{--Pruebas--}

--Debe regresar [1,6,3,5,8,3,4,4,4]
prueba1 = eliminaUno 4 [1,6,3,5,8,4,3,4,4,4]

--Debe regresar [4,(-3),8,5,5,(-10)]
prueba2 = eliminaUno (-10) [4,(-3),8,5,(-10),5,(-10)]

--Debe regresar [1,6,3,5,8,3]
prueba3 = eliminaTodos 4 [1,6,3,5,8,4,3,4,4,4]

--Debe regresar [4,(-3),8,5,5]
prueba4 = eliminaTodos (-10) [4,(-3),8,5,(-10),5,(-10)]

--Debe regresar [8,1,4,8]
prueba5 = tira 5 [4,3,5,6,7,8,1,4,8]

--Debe regresar [0,3]
prueba6 = tira 7 [5,7,4,8,12,3,6,0,3]

--Debe regresar [9,8,1,7,2,6,3,5]
prueba7 = reversa [5,3,6,2,7,1,8,9]

--Debe regresar [7,23,0,67,34,8,6]
prueba8 = reversa [6,8,34,67,0,23,7] 

--Debe regresar 13159190
prueba9 = sumaCuad 340

--Debe regresar 627874
prueba10 = sumaCuad 123
