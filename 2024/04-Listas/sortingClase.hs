{-# OPTIONS_GHC -fno-warn-tabs #-}
{-#LANGUAGE GADTs #-}

module Sorting where


-- LISTA ORDENADA

ordenada :: Ord a => [a] -> Bool
ordenada = \l -> case l of {[] -> True ;
							x:xs -> case xs of {
										[] -> True ;                  -- l = [x]
										y:ys -> x<=y && ordenada xs	} -- l = x:y:ys
							}
-- Recursion Primitiva en l												
												

ordenada2 :: Ord a => [a] -> Bool 
ordenada2 = \l -> case l of {[] -> True;
							 [x] -> True; 						 	-- l = [x];
							 x:y:ys -> x <= y && ordenada2 (y:ys)}	-- l = x:y:ys										

-- Recursion Estructural:											
 -- Varios casos base
 -- LLamada recursiva se hace sobre una SUBEXPRESION (string contenido sintácticamente) de la expresión original (En este caso "y:ys" está contenido en "x:y:ys") <- chequeo sintáctico.
 -- Por esto nos aseguramos que el programa TERMINA, las llamadas recursivas se hacen sobre elementos mas chicos.
 -- Para estar seguros que llegamos a alguno de los casos base debemos asegurarnos de no olvidarnos de ninguno: EXHAUSTIVIDAD: no falta ningún caso (hay que verificarlo en cada función)
 -- Para asegurarnos que estamos defiendo una funcion en forma correcta tenemos que asegurarnos que los casos sean MUTUAMENTE EXCLUYENTES (no se superponen las ramas en el case).



-- INSERT SORT 
-- Método: 
-- 1) Sacar un elemento de la lista (Trivial: sacamos el primero)
-- 2) Ordenar la lista sin el elemento que sacamos (Magia: llamada recursiva)
-- 3) Ponel el elemento que sacamos en su lugar (Función auxiliar: insert)


-- insert: Inserta un elemento en una lista ordenada y la deja ordenada
insert :: Ord a => a -> [a] -> [a]
insert = \e l -> case l of {[] -> [e];
							x:xs -> case e <= x of {True -> e : x : xs;
													False -> x : insert e xs}}


insertSort :: Ord a => [a] -> [a]
insertSort = \l -> case l of {[] -> [];
							  x:xs -> insert x (insertSort xs)}

-- Cómo se computa insertSort [7,4,2]?
-- insertSort [7,4,2]
-- = insert 7 (insertSort [4,2])
-- = insert 7 (insert 4 (insertSort [2]))
-- = insert 7 (insert 4 (insert 2 (insertSort [])))
-- = insert 7 (insert 4 (insert 2 [])))
-- = insert 7 (insert 4 [2])
-- = insert 7 (2 : insert 4 [])
-- = insert 7 [2,4]
-- = 2: insert 7 [4]
-- = 2: 4: insert 7 []
-- = 2: 4: [7] = [2,4,7] 



-- SELECT SORT
-- Método: 
-- 1) Sacar un elemento de la lista (sacamos el más chico: minL, sacar1)
-- 2) Ordenar la lista sin el elemento que sacamos (Magia: llamada recursiva)
-- 3) Ponel el elemento que sacamos en su lugar (Trivial: lo ponemos adelante de todo)

-- minL: Minimo de una lista no vacia
minL :: Ord a => [a] -> a
minL = \l -> case l of {[] -> error "lista vacia";
						[x] -> x;
						x:y:ys -> case x <= y of {True -> minL (x:ys) ; False -> minL (y:ys)}}

-- Recursion Estructural:
-- Terminación: las llamadas recursivas se hacen con x:ys y con y:ys que son ambas subexpresiones de x:y:ys
-- Los casos son exhaustivos (0, 1 y 2 o más elementos en l)
-- Los casos son mutuamente excluyentes


-- sacar1: elimina la primera ocurrencia de un elemento en una lista
sacar1 :: Eq a => a -> [a] -> [a]
sacar1 = \x l -> case l of {[] -> []; -- también podríamos haber puesto error "no se encuentra el elemento"
							w:ws -> case w == x of {True -> ws ; False -> w : sacar1 x ws}} 


selectSort :: Ord a => [a] -> [a] 
selectSort = \l -> case l of {[] -> [];				  
							  _  ->   minL l : selectSort (sacar1 (minL l) l)} 
							 
-- Por qué esta recursión termina?

-- RECURSION BIEN FUNDADA: las llamadas recursivas se hacen sobre objetos que son MENORES segun un orden BIEN FUNDADO 
-- Un orden es bien fundado si no hay cademas infinitas descendientes (por ejemplo el orden de los naturales)				  
-- Esta funcion termina porque: 
  -- la llamada recursiva se hace con "borrar1 (minL l) l", que SABEMOS  que tiene 1 elemento menos que l
  -- ya que le borramos el minimo, que SABEMOS que esta en l 
  
  -- Para justificar que esta funcion termina deberiamos demostrar dos resultados:
  -- L1 : not(null l) => elem (minL l) l = True
  -- L2 : elem e l => length(sacar1 e l) < length l 
							  

-- Como computa Haskell selectsort [3,5,1]???
-- selectsort [3,5,1]    (minL [3,5,1] = 1 y borrar1 1 [3,5,1] = [3,5])
-- 1: selectsort [3,5]    (minL [3,5] = 3 y borrar1 3 [3,5] = [5])						
-- 1: 3: selectsort [5]	   (minL [5] = 5 y borrar1 5 [5] = [])
-- 1: 3: 5: selectsort []
-- 1: 3: 5: [] = [1,3,5]




-- MERGE SORT 
-- Método: 
-- 1) Dividir la lista en dos (función split: "reparte" los elementos al igual que con una baraja de cartas)
-- 2) Ordenar ambas listas (Magia: llamadas recursivas)
-- 3) Unir las dos listas ordenadas (función merge)


-- split: Separa una lista en dos, poniendo los elementos uno-y-uno en cada lista
split :: [a] -> ([a],[a])  
split = \l -> case l of {[]->([],[]);
						 [x] -> ([x],[]);
						 x:y:zs -> case split zs of {(ys,ws) -> (x:ys,y:ws)}}
								 
-- Recursión estructural: Casos EX-EX
-- LLamadas recursivas con zs contenida en x:y:zs							  
								  

-- merge: Une dos listas ordenadas devolviendo una ordenada								  
merge :: Ord a => [a] -> [a] -> [a] 
merge = \l1 l2 -> case l1 of {[] -> l2 ;
							  x:xs -> case l2 of {[] -> l1 ;
												  y:ys -> case x<=y of {True -> x : merge xs l2; 
																		False -> y : merge l1 ys}}}
-- Recursión primitiva: no hay nada que justificar


mergeSort :: Ord a => [a] -> [a]
mergeSort = \l -> case l of {[]  -> [];
							 [x] -> [x];
							  _  -> case split l of {(xs,ys) -> merge (mergeSort xs) (mergeSort ys)}}

-- Recursión Bien Fundada
-- Tenemos que asegurarnos de que las listas que se obtienen del split son más pequeñas que la original
-- Eso no se cumple para la lista vacía ni para la lista unitaria, por lo que hacemos casos aparte
-- Cuando la lista l tiene por lo menos 2 elementos, sabemos que el split devuelve dos listas más pequeñas, 
-- y ahí las llamadas recursivas son correctas:
-- - L1 : length l >= dos => length (fst (split l)) < length l
-- - L2 : length l >= dos => length (snd (split l)) < length l
-- Los casos son exhaustivos, y como Haskell recorre las ramas del case de arriba para abajo, también son excluyentes.





-- QUICK SORT 
-- Método: 
-- 1) Dividir la lista en dos (los más chicos que la cabeza en una y los más grandes en otra)
-- 2) Ordenar ambas listas (Magia: llamadas recursivas)
-- 3) Unir las dos listas ordenadas (Trivial, con la cabeza de la lista original en el medio)


quickSort :: Ord a => [a] -> [a]
quickSort = \l -> case l of {[]  -> [];
							 x:xs -> quickSort(filter (<=x) xs) ++ x: quickSort(filter (>x) xs) }

-- Recursión Bien Fundada
-- las listas que se obtienen del filter p xs son más pequeñas que la original (x:xs), porque a lo sumo 
-- tienen todos los elementos de xs, pero el x no está
-- - L1: (Vx,Vxs,Vp) length (filter p xs) <= length xs < length (x:xs)
-- Los casos son EX-EX ([] y x:xs)