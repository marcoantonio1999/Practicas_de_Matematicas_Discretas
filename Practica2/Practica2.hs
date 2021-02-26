--Estructuras Discretas 2018-1
--Profesor: Laura Freidberg Gojman
--Ayudante: Ricardo Jiménez Méndez
--Práctica 2
--Alumno:Orduña Avila Marco Antonio
--No. de Cuenta: 315019928

--Calcula el volumen de un cono
volCono :: (Num a,Floating a) => a -> a -> a
volCono radio altura = ((3.1416*(radio*radio))* altura)/(3) 

--Calcula el area de un trapecio
areaTrap :: (Num a,Floating a) => a -> a -> a -> a
areaTrap basemayor basemenor altura = ((basemayor+basemenor)*altura)/(2)

--Te indica como eres dependiendo de tu edad
edad :: Int -> String
edad n 
	|n>=0 && n<=9 = "chamaco prro"
	|n>=10 && n<=20 = "puberto >:v"
	|n>=21 && n<=40 = "c nnor  :V"
	|n>=41 && n<=60 = "el futuro es hoy oiste viejo :V"
	|otherwise = "error 404"
																		
			
--Nos indica si un entero es par 
par :: Int -> Bool
par n =
	if  mod n 2==0
    then True
	else False
--Nos indica si un entero es impar
impar :: Int -> Bool
impar n =
	if mod n 2==1
	then True
	else False

{--Pruebas--}

--Debe regresar 1413.72
prueba1 = volCono 10 (13.5)

--Debe regresar 76328.6288072
prueba2 = volCono (-45.7) (34.9)

--Debe regresar 1038.78
prueba3 = areaTrap (13) (46.7) (34.8)

--Debe regresar 568.26
prueba4 = areaTrap (56.2) (34) (12.6)

--Debe regresar el mensaje que pusiste
prueba5 = edad (-5)

--Debe regresar el mensaje que pusiste
prueba6 = edad 46

--Debe dar True
prueba7 = par 2358

--Debe dar False
prueba8 = par (-367)

--Debe dar True
prueba9 = impar (-8925)

--Debe dar False
prueba10 = impar 123456
