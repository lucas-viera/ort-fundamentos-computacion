--clase 07/06/2022

{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-} 
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Sorting where


ordenada :: Ord a => [a] -> Bool
ordenada =  \l -> case l of {[] -> True;
                              x:xs -> case xs of {[] -> True;                           -- l = [x]
                                                  y:ys -> x<=y && ordenada (y:ys)}}     -- l = x:y:ys

{--RECURSION ESTRUCTURAL
* varios casos base
* llamada recursiva con expresiones estrictamente contenidas
en la expresion del caso correspondiente
--}


ordenada1 :: Ord a => [a] -> Bool
ordenada1 = \l -> case l of {[] -> True;
                             [x] -> True;
                              x:y:ys -> x<=y && ordenada1 (y:ys)}

{-- El poder expresivo de ambas formas de recursion es el mismo.
Ganamos legibilidad.
Para asegurarnos que la definicion es correcta, debemos asegurarnos 2 cosas:
- Exhaustividad: no falta ningun caso (lista vacio, 1 elemento, 2 o mas etc)
- Casos mutualmente excluyentes (no se superponen entre ellos)

--}


-- Ejercicio 2.b insertSort

-- 1- sacar un elemento de la lista (el primero)
-- 2- ordeno el resto de la lista mediante recursion
-- 3- pongo el elemento que saque en su lugar con una funcion auxiliar: insert

insertSort :: Ord a => [a] -> [a]
insertSort = \l -> case l of {[] -> [];
                            x:xs -> insert x (insertSort xs)}
                            --[5,2,9,1,3,0]           [0,1,2,3,9] sin 5

-- inserta un elemento a la lista ordenada y la deja ordenada
insert :: Ord a => a -> [a] -> [a]  
insert = \x l -> case l of {[] -> [x] ;
                            y:ys -> case x<=y of {True -> x:y:ys;
                                                  False -> y: insert x ys}}


-- Ejercicio 3: minL, borrar1, selectSort

-- 1- Saco el elemento mas chico de la lista
-- 2- Ordeno el resto de la lista mediante recursion
-- 3- Pongo el elemento que saque adelante que es el lugar correcto

minL :: Ord a => [a] -> a
minL = \l -> case l of { [] -> error "minL: lista vacia";
                      [x] -> x;
                      x:y:ys -> case x<y of { True -> minL (x:ys) ; False -> minL (y:ys) } 
                    }

borrar1 :: Eq a => a -> [a] -> [a]
borrar1 = \x l -> case l of {[] -> error "elemento no esta en la lista";
                             z:zs -> case x==z of {True -> zs; False -> z: borrar1 x zs}
                            }

selectSort :: Ord a => [a] -> [a]
selectSort = \l -> case l of {[] -> [];
                              _ -> minL l : selectSort (borrar1 (minL l) l) }

-- Ejercicio 4: split, merge, mergeSort

split :: [a] -> ([a], [a])
split = \l -> case l of { [] -> ([],[]);
                          [x] -> ([x],[]);
                          x:y:ys -> (x:fst(split ys), y:snd(split ys))}

merge :: Ord a => [a] -> [a] -> [a]
merge = \l1 l2 -> case l1 of {[] -> l2;
                              x:xs -> case l2 of {[] -> l1;
                                                  y:ys -> case x < y of {True -> x:merge(xs l2);
                                                                         False -> y:merge(l2 ys)}}}

mergeSort :: Ord a => [a] -> [a]
mergeSort = \l ->  case l of {[] -> [];
                              [x] -> [x];
                              _ -> case split l of {(l1,l2) -> merge (mergeSort l1) (mergeSort l2)}