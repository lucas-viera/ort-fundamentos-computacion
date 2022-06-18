{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

data ABB a where { Vacio :: ABB a ; 
					Unir :: ABB a -> a -> ABB a -> ABB a} deriving Show
					
t :: ABB Int
t = Unir (Unir (Unir Vacio 1 Vacio) 3 (Unir Vacio 4 Vacio)) 6 (Unir Vacio 8 Vacio)

ordenado:: Ord a => ABB a -> Bool
ordenado = undefined

inOrder :: ABB a -> [a]
inOrder = \t -> case t of {Vacio -> [];
						  Unir t1 x t2 -> inOrder t1 ++ x : inOrder t2}
						  
insertABB :: Ord a => a -> ABB a -> ABB a
insertABB = \x t -> case t of {Vacio -> Unir Vacio x Vacio;
							  Unir t1 z t2 -> case x <= z of {True -> Unir (insertABB x t1) z t2;
															False -> Unir t1 z (insertABB x t2)}}
															
list2ABB :: Ord a => [a] -> ABB a
-- inserta 1 a 1 los elementos de una lista en un ABB ordenado
list2ABB = \l -> case l of {[] -> Vacio;
							x:xs -> insertABB x (list2ABB xs)}


treeSort :: Ord a => [a] -> [a]
treeSort = \l -> inOrder (list2ABB l)


-------------Expresiones aritméticas

data Exp where {Num :: Int -> Exp ; -- LAS HOJAS TIENEN UN NÚMERO COMO INFORMACIÓN
				Sum :: Exp -> Exp -> Exp ;
				Mul :: Exp -> Exp -> Exp} deriving Show
				
e :: Exp
e = Sum (Num 3)(Mul (Num 4)(Num 5))

eval :: Exp -> Int
eval = \e -> case e of {Num x -> x ;
						Sum e1 e2 -> eval e1 + eval e2;
						Mul e1 e2 -> eval e1 * eval e2}








