Python 3.6.3 (v3.6.3:2c5fed8, Oct  3 2017, 18:11:49) [MSC v.1900 64 bit (AMD64)] on win32
Type "copyright", "credits" or "license()" for more information.
>>> """
Funcion que calcula la distancia entre dos puntos
"""
'\nFuncion que calcula la distancia entre dos puntos\n'

>>> def distancia (x1,x2,y1,y2):
	x1 = imput ("ingresa el punto x1 :")
	x2 = imput ("ingresa el punto x2 :")
	y1 = imput ("ingresa el punto y1 :")
	y2 = imput ("ingresa el punto y2 :")
	disPuntos = ((x2-x1)**2+(y2-y1)**2)**(1/2)
	return disPuntos
	print ("la distancia entre los dos puntos es : "disPuntos)
	
>>> """
Funcion que calcula el area de un circulo
"""

'\nFuncion que calcula el area de un circulo\n'
>>> def areaCirculo (r):
	r = imput ("ingresa el radio")
	areaCir = (r*r)*3.1416
	return areaCir
	print ("el area del circulo es :"areaCir)

>>> 
"""
 Funcion que nos da el volumen de un cono
"""
' Funcion que nos da el volumen de un cono\n'
>>> def VolCono (r,h):
	r = imput ("ingresa el radio ")
	h = imput ("ingresa la altura")
	VolumenCono = ((3.1416(r*r))*h)/3
	return VolumenCono
	print ("el volumen del cono es :"VolumenCono)
>>> """
Funcion que te dice como eres dependiendo tu edad
"""
'\nFuncion que te dice como eres dependiendo tu edad\n'
>>> def edad (i):
	i = imput("ingresa tu edad")
	if (i<0):
		print("eres un pinche chamaco xddd")
	elif (i<12):
		print("eres mi chavo jaja ")
	elif (i<18):
		print("ya eres cancha reglamentaria")
	elif (i<40):
		print ("pinche ruco we ")
	else :
		print("saca las panoshaaas prro")

		
>>> """
Funcion que elimina el primer elemento de uno que recibe
"""
'\nFuncion que elimina el primer elenmento de uno que recibe\n'
 def eliminaUno (n,(l))
	n = imput("ingresa el numero que quieres eliminar")
	(l) = imput ("ingresa la lista a la que le quieres quitar el elementos")
	if ((l) == []):
		return []
		print []
	elif (n == l[0]):
		del l[0]
		return (l)
		print (l)
	else: 
		e = l[0]
		del l[0]
		print ([e] + eliminaUno (n,(l)))
		
"""
funcion eliminaTodos de la practica 4
"""
def eliminaTodos (n,(l)):
	n = imput ("ingresa el entero que quieres que quite")
	(l) = ("ingresa la lista a la que quieres quitar los elemntos conforme a tu entero")
	if ((l) == [])
		return print []
	elif (n == l[0]):
		del l[0]
		return  print (eliminaTodos (n,(l)))
	else :
		e = l[0]
		del l[0]
		print ([e] + eliminaTodos (n,(l)))

>>> 
""" 
funcion tira de la practica 4
"""
>>> def tira (n,(l)):
	n = imput ("ingresa los elmentos que quieres quitar de la lsita)
	l = imput ("ingresa la lista a la que quieres quitar los elmentos)
	if ((l) == []):
		return  print("stas wey xddd")
	elif (n >= 1):
		e= l[0]
		del l[0]
	 	 return print ([] + tira (n-1,(l)))
	elif (tira(0,(l)):
		return print []
	else: 
		print("error")
    
		


""" 
funcion que nos saca el promedio de una lista 
"""

>>>	def promediarLista(l):
    l = imput("ingresa la lista a la que quieres sacar el promedio")
	sum = 0.0
    for i in range(0,len(l)):
        sum = sum+l[i]
 
    print sum/len(l)

""" 
funcion cuentaNum de la practica 4 
"""
def ceuntaNum(n,l):
	cuen = 0
	for i in range(len(l)):
		if (n==l[1]):
			cuent = cuent + 1
	print cuent
	
"""
	Funcion que nos calcula el fibonacci
"""
def fib(n):
    a, b = 0,1
    while a < n:
        print(a, end=' ')
        a, b = b, a+b
    print()
 fib(1000)
