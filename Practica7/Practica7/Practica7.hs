
module Practica7 where

--Estructuras Discretas 2018-1
--Profesor: Laura Freidberg Gojman
--Ayudante: Ricardo Jiménez Méndez
--Práctica 7
--Alumno:Orduña AVila Marco Antonio
--No. de Cuenta: 315019928

--Se define el tipo de dato Tree como
data Arbol a = Vacio                        --Árbol vacío
             | Nodo a (Arbol a) (Arbol a)   --Nodo con sub-árbol izquierdo y sub-árbol derecho
             deriving (Show,Eq)

--Regresa el número de nodos de un árbol
noNodos :: Arbol a -> Int
noNodos Vacio = 0 
noNodos (Nodo a m1 m2) = 1 + (noNodos m1) + (noNodos m2)

			


--Regresa el número de hojas de un árbol
noHojas :: (Eq a) => Arbol a -> Int
noHojas Vacio = 0
noHojas (Nodo a m1 m2)= if (m1 == Vacio ) && (m2 == Vacio)
			then 1 + (noHojas m1) + (noHojas m2 )
			else 0 + (noHojas m1) + (noHojas m2 )

--Regresa el número de nodos internos de un árbol
noInt :: (Eq a) => Arbol a -> Int
noInt (Nodo a m1 m2) = (noNodos(Nodo a m1 m2)) - (noHojas(Nodo a m1 m2))

--Regresa el número de aristas de un árbol
aristas :: (Eq a) => Arbol a -> Int
aristas (Nodo a m1 m2) = (noNodos (Nodo a m1 m2)) -1

--Nos regresa los nodos internos de un árbol en una lista
nodosInt :: (Eq a) => Arbol a -> [a]
nodosInt Vacio = []
nodosInt (Nodo a m1 m2)= if (m1 == Vacio ) && (m2 == Vacio)
			then [] ++ (nodosInt m1) ++ (nodosInt m2)
			else [a] ++ (nodosInt m1) ++ (nodosInt m2)


--Nos regresa las hojas de un árbol en una lista
hojas :: (Eq a) => Arbol a -> [a]
hojas Vacio = [] 
hojas (Nodo a m1 m2) = if (m1 == Vacio) && (m2 == Vacio)
			then [a] ++ (hojas m1) ++ (hojas m2)
			else [] ++ (hojas m1) ++ (hojas m2) 

--Nos dice si el nodo que estamos buscando se encuentra en el árbol dado
buscaNodo :: (Eq a) => a -> Arbol a -> Bool
buscaNodo n Vacio = False
buscaNodo n (Nodo a m1 m2) = if n == a
				then True
				else buscaNodo n m1 || buscaNodo n m2 

  

--Hace un recorrido inorder a un árbol y regresa el recorrido en una lista
inorder :: Arbol a -> [a]
inorder Vacio = []
inorder (Nodo a m1 m2) = (inorder m1) ++ [a] ++ (inorder m2)


--Hace un recorrido preorder a un árbol y regresa el recorrido en una lista
preorder :: Arbol a -> [a]
preorder Vacio = []
preorder  (Nodo a m1 m2 ) = [a] ++ (preorder m1) ++ (preorder m2) 

--Hace un recorrido posorder a un árbol y regresa el recorrido en una lista
posorder :: Arbol a -> [a]
posorder Vacio = []
posorder (Nodo a m1 m2) = (posorder m1) ++ (posorder m2) ++ [a]

{--pruebas--}

--Debe regresar 10
prueba1 = noNodos (Nodo 1
                    (Nodo 2
                      (Nodo 9
                        (Vacio)
                        (Nodo 10 (Vacio) (Vacio))
                      )
                      (Nodo 4
                        (Nodo 5 (Vacio) (Vacio))
                        (Nodo 6 (Vacio) (Vacio))
                      )
                    )
                    (Nodo 3
                      (Nodo 7 (Vacio) (Vacio))
                      (Nodo 8 (Vacio) (Vacio))
                    )
                  )

--Debe regresar 4
prueba2 = noHojas (Nodo 1
                    (Nodo 2
                      (Vacio)
                      (Nodo 4
                        (Nodo 5 (Vacio) (Vacio))
                        (Nodo 6 (Vacio) (Vacio))
                      )
                    )
                    (Nodo 3
                      (Nodo 7 (Vacio) (Vacio))
                      (Nodo 8 (Vacio) (Vacio))
                    )
                  )

--Debe regresar 5
prueba3 = noInt (Nodo 1
                     (Nodo 2
                       (Nodo 9
                         (Vacio)
                         (Nodo 10 (Vacio) (Vacio))
                       )
                       (Nodo 4
                         (Nodo 5 (Vacio) (Vacio))
                         (Nodo 6 (Vacio) (Vacio))
                       )
                     )
                     (Nodo 3
                       (Nodo 7 (Vacio) (Vacio))
                       (Nodo 8 (Vacio) (Vacio))
                     )
                   )

--Debe regresar 9
prueba4 = aristas (Nodo 1
                    (Nodo 2
                      (Nodo 9
                        (Vacio)
                        (Nodo 10 (Vacio) (Vacio))
                      )
                      (Nodo 4
                        (Nodo 5 (Vacio) (Vacio))
                        (Nodo 6 (Vacio) (Vacio))
                      )
                    )
                    (Nodo 3
                      (Nodo 7 (Vacio) (Vacio))
                      (Nodo 8 (Vacio) (Vacio))
                    )
                  )

--Debe regresar [1,2,9,4,3]
prueba5 = nodosInt (Nodo 1
                    (Nodo 2
                     (Nodo 9
                      (Vacio)
                      (Nodo 10 (Vacio) (Vacio))
                     )
                     (Nodo 4
                      (Nodo 5 (Vacio) (Vacio))
                      (Nodo 6 (Vacio) (Vacio))
                     )
                    )
                    (Nodo 3
                     (Nodo 7 (Vacio) (Vacio))
                     (Nodo 8 (Vacio) (Vacio))
                    )
                   )
 
--Debe regresar [5,6,3]
prueba6 = hojas (Nodo 1
                  (Nodo 2
                    (Vacio)
                    (Nodo 4
                      (Nodo 5 (Vacio) (Vacio))
                      (Nodo 6 (Vacio) (Vacio))
                    )
                  )
                  (Nodo 3
                    (Vacio)
                    (Vacio)
                  )
                )

--Debe regresar True
prueba7 = buscaNodo 5 (Nodo 1
                        (Nodo 2
                          (Vacio)
                          (Nodo 4
                            (Nodo 5 (Vacio) (Vacio))
                            (Nodo 6 (Vacio) (Vacio))
                          )
                        )
                        (Nodo 3
                          (Nodo 7 (Vacio) (Vacio))
                          (Nodo 8 (Vacio) (Vacio))
                        )
                      ) 

--Debe regresar [9,10,2,5,4,6,1,7,3,8]
prueba8 = inorder (Nodo 1
                    (Nodo 2
                      (Nodo 9
                       (Vacio)
                       (Nodo 10 (Vacio) (Vacio))
                       )
                      (Nodo 4
                        (Nodo 5 (Vacio) (Vacio))
                        (Nodo 6 (Vacio) (Vacio))
                      )
                    )
                    (Nodo 3
                      (Nodo 7 (Vacio) (Vacio))
                      (Nodo 8 (Vacio) (Vacio))
                    )
                  )

--Debe regresar [1,2,9,10,4,5,6,3,7,8]
prueba9 = preorder (Nodo 1
                     (Nodo 2
                       (Nodo 9
                         (Vacio)
                         (Nodo 10 (Vacio) (Vacio))
                       )
                       (Nodo 4
                         (Nodo 5 (Vacio) (Vacio))
                         (Nodo 6 (Vacio) (Vacio))
                       )
                     )
                     (Nodo 3
                       (Nodo 7 (Vacio) (Vacio))
                       (Nodo 8 (Vacio) (Vacio))
                     )
                   )

--Debe regresar [10,9,5,6,4,2,7,8,3,1]
prueba10 = posorder (Nodo 1
                      (Nodo 2
                        (Nodo 9
                          (Vacio)
                          (Nodo 10 (Vacio) (Vacio))
                        )
                        (Nodo 4
                          (Nodo 5 (Vacio) (Vacio))
                          (Nodo 6 (Vacio) (Vacio))
                        )
                      )
                      (Nodo 3
                        (Nodo 7 (Vacio) (Vacio))
                        (Nodo 8 (Vacio) (Vacio))
                      )
                    )
