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
				Mul :: Exp -> Exp -> Exp ;
				Op :: Exp -> Exp} deriving Show
				
e :: Exp
e = Sum (Num 3)(Mul (Num 4)(Num 5))

e1 :: Exp -- (2 + 3) * (4 * (5 + 1))
e1 = Mul (Sum (Num 2)(Num 3))(Mul (Num 4)(Sum (Num 5)(Num 1)))

e2:: Exp -- -(1+3) * (-(2*2))
e2 = Mul (Op (Sum (Num 1)(Num 3)))(Op(Mul(Num 2)(Num 2)))

eval :: Exp -> Int
eval = \e -> case e of {Num x -> x ;
						Sum e1 e2 -> eval e1 + eval e2;
						Mul e1 e2 -> eval e1 * eval e2;
						Op e1 -> - (eval e1)}
						
set :: Exp -> Int -> Exp
-- cambia todas las hojas de una Exp por un número dado
set = \e n -> case e of {Num x ->  Num n;
						Sum e1 e2 -> Sum (set e1 n) (set e2 n);
						Mul e1 e2 -> Mul (set e1 n) (set e2 n);
						Op e1 -> Op (set e1 n) }
							
{-- (∀e :: Exp) eval(set e 0) = 0
Dem. Por inducción en e::Exp
Caso Base: e = Num x, con x::Int cualquiera
eval(set (Num x) 0) =? 0	

	eval(set (Num x) 0)
	= (def. set)
	eval (Num 0)
	= (def. eval)
	0

Casos inductivos

e = Sum e1 e2, con e1, e2::Exp cualesquiera
HI1) eval(set e1 0) = 0
HI2) eval(set e2 0) = 0
TI) eval (set (Sum e1 e2) 0) =? 0
	
	eval (set (Sum e1 e2) 0)
	= (def. set)
	eval (Sum (set e1 0)(set e2 0))
	= (def. eval)
	eval (set e1 0) + eval (set e2 0)
	= (HI1 y HI2)
	0 + 0
	= (def. +)
	0
	
e = Mul e1 e2, con e1, e2::Exp cualesquiera
HI1) eval(set e1 0) = 0
HI2) eval(set e2 0) = 0
TI) eval (set (Mul e1 e2) 0) =? 0	
	
	eval (set (Mul e1 e2) 0)
	= (def. set)
	eval (Mul (set e1 0)(set e2 0))
	= (def. eval)
	eval (set e1 0) * eval (set e2 0)
	= (HI1 y HI2)
	0 * 0
	= (def. *)
	0
	
e = Op e1, con e1::Exp cualquiera
HI) eval(set e1 0) = 0
TI) eval (set (Op e1) 0) =? 0

	eval (set (Op e1) 0)
	= (def. set)
	eval (Op (set e1 0))
	= (def. eval)
	- (eval (set e1 0))
	= (HI)
	- 0
	= (resultado matemático que me dan en la letra)
	0




data Exp where {Num :: Int -> Exp ; 
				Sum :: Exp -> Exp -> Exp ;
				Mul :: Exp -> Exp -> Exp ;
				Op :: Exp -> Exp} deriving Show
				
				--}
				
				
				
-- Ej9

data BT where { C :: BT ;
				U :: BT ;
				POR :: [BT] -> BT ;
				MAS :: [BT] -> BT } deriving Show
				
				
				
bt :: BT
bt = POR [MAS[U,C,POR[C,U]] , U ]
 
unos :: BT -> Int
unos = \t -> case t of {C -> 0;
						U -> 1;
						POR ts -> case ts of {[] -> 0 ; a:as -> unos a + unos (POR as)};
						MAS ts -> sum (map unos ts)}


valor :: BT -> Bool
valor = \t -> case t of {C -> False;
						U -> True;
						POR ts -> case ts of {[] -> True ; a:as -> valor a && valor (POR as) };
						MAS ts -> and (map valor ts)}
						
						
						
						
						
						
						
						
						










