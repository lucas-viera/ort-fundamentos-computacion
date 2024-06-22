{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use lambda" #-}
{-# HLINT ignore "Use last" #-}
{-# HLINT ignore "Use init" #-}
{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Use first" #-}

module Listas where
import Prelude hiding (all, and, any, concat, drop, dropWhile, elem, filter, fst, head, init, last, length, map, null, or, reverse, snd, split, sum, tail, take, takeWhile, zip, zipWith, (!!), (++))
import Naturales
import GHC.Base (Opaque(O))
import Naturales (N)
-- 1 Listas
null :: [a] -> Bool
null = \l -> case l of {
                        [] -> True;
                        x:xs -> False
                        }
length :: [a] -> N
length = \l -> case l of {
                            [] -> O;
                            x:xs -> S(length xs)
                          }

duplicate :: [a] -> [a]
duplicate = \l -> case l of {
                              [] -> [];
                              x:xs -> x:x:(duplicate xs)
                            }

sum :: [N] -> N
sum = \l -> case l of {
                        [] -> O;
                        x:xs -> x + sum(xs)
                      }
  
prod :: [N] -> N
prod = \l -> case l of {
                          [] -> S O;
                          x:xs -> x * (prod xs)
                       }

map :: (a -> b) -> [a] -> [b]
map = \f l -> case l of {
                              [] -> [];
                              x:xs -> f x : map f xs
                            }

zipWith :: (a -> b -> c) -> [a] -> [b]
zipWith = \f l1 l2 -> case l1 of {
                                  [] -> [];
                                  x:xs -> case l2 of {
                                                        [] -> [];
                                                        y:ys -> f x y : zipWith f xs ys
                                                      }
                                  }
filter :: (a -> Bool) -> [a] -> [b]
filter = \p l -> case l of {
                            [] -> [];
                            x:xs -> case p x of {
                                                  False -> filter p xs;
                                                  True -> x : filter p xs
                                                }
                          }

and :: [Bool] -> Bool
and = \l -> case l of {
                        [] -> True;
                        x:xs -> x && (and xs)
                      }

or :: [Bool] -> Bool
or = \l -> case l of {
                        [] -> False;
                        x:xs -> x || (or xs)
                     }

count :: (a -> Bool) -> [a] -> N
count = \p l -> case l of {
                              [] -> O;
                              x:xs -> case p x of {
                                                    False -> count p xs;
                                                    True -> S(count p xs)
                                                  }
                          }

any :: (a -> Bool) -> [a] -> Bool
any = \p l -> case l of { 
                          [] -> False;
                          x:xs -> case p x of {
                                                False -> any xs;
                                                True -> True
                                              }
                        }
                        
--otra forma any
any2 :: (a -> Bool) -> [a] -> Bool
any2 = \p l -> case l of {
                            [] -> False;
                            x : xs -> (p x) || (any p xs)
                          }

all :: (a -> Bool) -> [a] -> Bool
all = \p l -> case l of {
                          [] -> True;
                          x:xs -> case p x of {
                                                False -> False;
                                                True -> (p x) && (all p xs)
                                              }
                        }

-- otra forma all
all2 :: (a -> Bool) -> [a] -> Bool
all2 = \p l -> case l of {
                            [] -> True;
                            x : xs -> (p x) && (all p xs)
                         }

-- redefiniciones sin usar case
count1 :: (a -> Bool) -> [a] -> N
count1 = \p l -> length (filter p l)

any3 :: (a -> Bool) -> [a] -> Bool
any3 = \p l -> (length (filter p l)) >= (S O)

all3 :: (a -> Bool) -> [a] -> Bool
all3 = \p l -> (length (filter p l)) == (length l)
 
--concatena dos listas 
(++) :: [a] -> [a] -> [a]
(++) = \l1 l2 -> case l1 of {
                              [] -> l2;
                              x:xs -> x: xs ++ l2
                            }

reverse :: [a] -> [a]
reverse = \l -> case l of {
                            [] -> [];
                            x:xs -> (reverse xs) ++ x
                          }

-- verifica si un elemento pertenece a una lista
elem::Eq a => a -> [a] -> Bool
elem = \e l -> case l of {
                            [] -> False;
                            x:xs -> (e == x) || (elem e xs)
                          }

--concatena una lista de listas
concat :: [[a]] -> [a]
concat = \l -> case l of {
                           [] -> [];
                           xs:xss -> (xs) ++ (concat xss)
                         }

--suma los largos de las listas de una lista de listas
lensum :: [[a]] -> N
lensum = \l -> case l of {
                            [] -> O;
                            xs:xss -> (length xs) + (lensum xs)
                          }

--retorna el indice del ulitmo elemento que el predicado se cumple
ultimo :: [a] -> (a -> Bool) -> N
ultimo = undefined

-- 2 Pares ordenados
-- data (a,b) where { (_,_) :: a -> b -> (a,b)}

-- retorna primer componente del par ordenado
fst :: (a,b) -> a
fst = \par -> case par of {(x,y) -> x}

-- retorna segundo componente del par ordenado
snd :: (a, b) -> a
snd = \par -> case par of {(x,y) -> y}

-- devuelve los pares de elementos que aparecen en la misma posicion en ambas listas. Hasta que una de las dos se acabe
zip :: [a] -> [b] -> [(a,b)]
zip = \l1 l2 -> case l1 of {
                              [] -> [];
                              x:xs -> case l2 of {
                                                    [] -> [];
                                                    y:ys -> (x,y) : zip (xs, ys)
                                                  }
                            }

-- recibe una lista y un valor, separa en 2 listas segun si son mayores o menores que un valor dado
menMay :: Ord a => [a] -> a -> ([a],[a])
menMay = \l valor -> case l of {
                                  [] -> ([],[]);
                                  x:xs -> case (x < valor) of {
                                                                False -> (fst(menMay xs valor), x: snd(menMay xs valor));
                                                                True -> (x:fst(menMay xs valor), snd(menMay xs valor))
                                                              }
                               }

--otra forma con menos llamado recursivo
menMay2 :: Ord a => [a] -> a -> ([a],[a])
menMay2 = \l valor -> case l of {
                                  [] -> ([],[]);
                                  x:xs -> case (x < valor) of {
                                                                False -> case menMay xs e of {(a,b) -> (a, x : b)};
                                                                True -> case menMay xs e of {(a,b) -> (x:a,b)}
                                                              }
                               }

-- 3 Funciones parciales
pre :: Bool -> String -> a -> al
pre = \b s x -> case b of {
                            False -> error s;
                            True -> x
                          }

head :: [a] -> a
head = \l -> case l of {
                          [] -> error "head: lista vacia";
                          x:xs -> x
                       }

--usando pre
tail :: [a] -> [a]
tail = \l -> pre(not(null l)) "tail: lista vacia" (case l of {x:xs -> xs})

--obtener el ultimo elemento de una lista no vacia
last :: [a] -> a
last = \l -> pre(not(null l)) "last: lista vacia" (head(reverse l))

--quitar el ultimo elemento a una lista no vacia
init :: [a] -> [a]
init = \l -> pre(not(null l)) "init: lista vacia" (reverse(tail(reverse l)))

-- devolver el n-esimo elemento de una lista no vacia (empeznao desde 0)
(!!) :: [a] -> N -> a
(!!) = undefined

-- devolver el minimo elemento de una lista
minList :: Ord a => [a] -> a
minList = undefined

-- 4 Mas funciones sobre listas