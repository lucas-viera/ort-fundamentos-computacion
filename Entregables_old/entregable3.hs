{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Entregable3 where


data N where {O :: N; S :: N -> N}
  deriving (Show)


uno::N
uno = S O

dos :: N
dos = S uno
	
tres :: N
tres = S dos

cuatro = S tres
cinco = S cuatro


par :: N -> Bool
par = \n -> case n of{O -> True; S k -> not (par k)}

-- Ej 1
mitad :: N -> N
mitad = \n -> case n of {O -> O;
						S x -> case par n of {True -> S(mitad x) ; False -> mitad x}}

-- Ej 2						
mini :: N -> N -> N
mini = \m n -> case m of {O -> O;
						S x -> case n of {O -> O ; S y -> S(mini x y)}}						

instance Eq N where
	(==) = \n m -> case n of{
				O -> case m of {O -> True ; S x -> False};
				S x -> case m of {O -> False ; S y -> x == y }}

-- Ej 3				
iguales:: N -> (N -> N) -> Bool
iguales = \n f -> case n of {O -> False;
							S x -> f n == f x  || iguales x f}						
						
instance Ord N where
	(<=) = \n m -> case n of{
				O -> True;
				S x -> case m of {O -> False ; S y -> x <= y}} 	

-- Ej 4
minf:: N -> (N -> N) -> N
minf = \n f -> case n of {O -> f O;
						  S x -> case f n <= minf x f of {True -> f n ; False -> minf x f}}									


						  
-- para las pruebas
instance Num N where
 (+) = \m n -> case m of {O -> n ;
						  S x -> S (x + n)}
 (*) = \m n -> 	case m of {O -> O ;
						  S x -> n + (x * n)}	
 (-) = \m n -> case n of {O -> m ;
						  S x -> case m of {O -> O ; S y -> y - x} }
						  
predecesor :: N -> N
predecesor = \n -> case n of{O -> O; S k -> k}

doble :: N -> N
doble = \n -> case n of{O -> O; S k -> S (S (doble k))}							  

alt :: N -> N
alt = \n -> case n of {O-> uno ; S x -> uno - alt x}
	
