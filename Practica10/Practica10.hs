module Practica10 where
import Data.List
{--
Estructuras Discretas 2018-1
Profesor: Laura Freidberg Gojman
Ayudante: Ricardo Jiménez Méndez
Práctica 10
Alumno:Orduña Avila Marco Antonio
No. de Cuenta:315019928
--}

--Tipo para asignar la estructura de una relacion
type Relacion = [(Char,Char)]
  

prodCart :: [Char] -> [Char] -> Relacion
prodCart (x:xs) (y:ys) = [ (x,y) | x<-(x:xs),y<-(y:ys)]

interseccion :: (Eq a)=> [a] -> [a] -> [[a]]
interseccion (x:xs) (y:ys) = [[x]|x<- (x:xs)`intersect`(y:ys)]

uniones :: [Char] -> [Char] ->[[Char]]
uniones (x:xs) (y:ys)= [[x] | x<-(x:xs)`union`(y:ys)]

diferencia ::[Char] -> [Char]-> [[Char]]
diferencia [] (y:ys) = []
diferencia (x:xs) (y:ys) = if (exist (x) (y:ys)==False)
                    then [[x]]++diferencia xs (y:ys)
                    else diferencia xs (y:ys)

--Nos dice si el elemento esta en la lista
exist :: (Eq a)=> a ->[a] -> Bool
exist m (x:xs)= m`elem`(x:xs)

--Une toda la lista continua
continua :: [Char] -> [Char] ->[Char]
continua (x:xs) (y:ys)= [x | x<-(x:xs)`union`(y:ys)]

--Interseccion continua
interConj :: (Eq a)=> [a] -> [a] -> [a]
interConj (x:xs) (y:ys) = [x| x <- (x:xs)`intersect`(y:ys) ]


--Elimina las variables repetidas
elimina :: Eq a => [a]->[a]
elimina [] = []
elimina (x:xs)= x : elimina (filter (/=x) xs)
-- Funcion que nos saca la diferencia simetrica
difSimetrica :: [Char]->[Char]->[[Char]]
difSimetrica (x:xs) (y:ys) = diferencia (continua (x:xs) (y:ys)) (interConj(x:xs)(y:ys))
 --Funcion que nos dice si un conjunto es reflexivo
reflexiva :: Relacion ->Bool
reflexiva []=error "La lista esta vacia"
reflexiva xs = and[(elem (n,n) xs) && (elem(m,m) xs)|(n,m)<-xs]

--Funcion que nos dice si un conjunto es simetrico
simetrica :: Relacion -> Bool
simetrica []= error "La lista esta vacia"
simetrica xs = and[elem(b,a) xs|(a,b)<-xs]  
--Funcion que nos dice si un conjunto es transitivo
trans :: Relacion ->Bool
trans[] = error "La lista esta vacia"
trans xs = and[(elem (n,o) xs)|(n,m)<-xs,(m,o)<-xs]

--Devuelve [('P','P'),('P','Q'),('P','R'),('Q','P'),('Q','Q'),('Q','R'),('R','P'),('R','Q'),('R','R')]
prueba1 = prodCart ['P','Q','R'] ['P','Q','R']

--Devuelve ["X","Y","Z","O"]
prueba2 = uniones ['X','Y','Z'] ['O','Y','X']

--Devuelve ["A","I","O"]
prueba3 = interseccion ['A','E','I','O','U'] ['W','O','J','I','A']

--Devuelve ["M","N","Q"]
prueba4 = diferencia ['M','N','O','P','Q'] ['R','Y','O','P']

--Devuelve ["A","B","E","F","H","I"]
prueba5 = difSimetrica ['A','B','C','D','E','F','G','H','I'] ['C','D','G']
--Devuelve True
prueba6 = reflexiva prueba1
--Devuelve False
prueba7 = reflexiva (prodCart ['A','B','C'] ['A','B'])
--Devuelve True 
prueba8 = simetrica [('P','P'),('P','Q'),('P','R'),('Q','P'),('Q','Q'),('Q','R'),('R','P'),('R','Q'),('R','R')]

--Devuelve False
prueba9= simetrica (prodCart ['P','Q','R'] ['P','Q'])
--Devuelve True 
prueba10= trans [('P','P'),('P','Q'),('P','R'),('Q','P'),('Q','Q'),('Q','R'),('R','P'),('R','Q'),('R','R')]
--Devuelve True 
prueba11= trans (prodCart ['P','Q','R'] ['P','Q'])







