{-# OPTIONS_GHC -fno-warn-tabs #-}
{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use lambda" #-}

module Sorting where


-- LISTA ORDENADA

-- recursion prmitiva en l
ordenada :: Ord a => [a] -> Bool
ordenada = \l -> case l of { [] -> True;                        --lista vacia []
                              x:xs -> case xs of { [] -> True;  --lista 1 elemento [a] 
                                                    y:ys -> x<=y && ordenada xs}}								
												
-- recursion estructural
ordenada2 :: Ord a => [a] -> Bool 
ordenada2 = \l -> case l of { [] -> True;
                              [x] -> True;
                              x:y:ys -> x<=y && ordenada2 (y:ys)}


insert :: Ord a => a -> [a] -> [a]
insert = \e l -> case l of { [] -> [e];
                              x:xs -> case e<=x of { True -> e:x:xs;
                                                     False -> x:insert e xs}}

insertSort :: Ord a => [a] -> [a]
insertSort = \l -> case l of { [] -> [];
                               x:xs -> insert x (insertSort xs)}




-- minL: Minimo de una lista no vacia
minL :: Ord a => [a] -> a
minL = \l -> case l of { [] -> error "lista vacia";
                          [x] -> x;
                          x:y:ys -> case x<=y of { False -> minL(y:ys);
                                                   True -> minL(x:ys)}}

-- sacar1: elimina la primera ocurrencia de un elemento en una lista
sacar1 :: Eq a => a -> [a] -> [a]
sacar1 = \e l -> case l of { [] -> error "no se encuentra el elemento";
                              x:xs -> case e==x of { False -> e:sacar1 e xs;
                                                     True -> xs}}

selectSort :: Ord a => [a] -> [a] 
selectSort = \l -> case l of { [] -> [];
                               _ -> minL l : selectSort (sacar1 (minL l) l)}

-- split: Separa una lista en dos, poniendo los elementos uno-y-uno en cada lista
split :: [a] -> ([a],[a])  
split = \l -> case l of { [] -> ([],[]);
                          [x] -> ([x],[]);
                          x:y:zs -> case split zs of { (xs, ys) -> (x:xs,y:ys)}}
								 
-- merge: Une dos listas ordenadas devolviendo una ordenada								  
merge :: Ord a => [a] -> [a] -> [a] 
merge = \l1 l2 -> case l1 of { [] -> l2;
                                x:xs -> case l2 of { [] -> l1;
                                                      y:ys -> case x<=y of { False -> y: merge l1 ys;
                                                                             True -> x: merge xs l2}}}


mergeSort :: Ord a => [a] -> [a]
mergeSort = \l -> case l of { [] -> [];
                               [x] -> [x];
                               _ -> case split l of { (xs, ys) -> merge (mergeSort xs) (mergeSort ys)}}


quickSort :: Ord a => [a] -> [a]
quickSort = \l -> case l of { [] -> [];
                              x:xs -> quickSort(filter (<=x) xs) ++ x: quickSort(filter (>x) xs)}
