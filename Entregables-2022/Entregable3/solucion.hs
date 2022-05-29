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

instance Eq N where
	(==) = \n m -> case n of{
				O -> case m of {O -> True ; S x -> False};
				S x -> case m of {O -> False ; S y -> x == y }}

instance Ord N where
	(<=) = \n m -> case n of{
				O -> True;
				S x -> case m of {O -> False ; S y -> x <= y}} 	


-- Ej 1
distancia :: N -> N -> N
distancia = \x y  -> case x of {O -> y ; S i -> case y of {O -> x ; S j -> distancia i j}}

-- Ej 2						
escalera :: N -> N -> N -> Bool
escalera = \x y z -> case x of {O -> case y of {O -> True ; S m -> case z of {O -> False ; S n -> escalera O m n}};
						       S k -> case y of {O -> case z of {O -> True ; S n -> False}; 
											   S m -> case z of {O -> escalera k m O ; S n -> escalera k m n}}}

-- Ej 3				
dosIguales :: (N -> Bool) -> N -> N
dosIguales = \f n -> case n of {O -> O;
						S x -> case f (S x) == f x of {True -> S x; False -> dosIguales f x}}

-- Ej 4
ninguno:: (N -> Bool) -> N -> Bool
ninguno = \f n -> case n of {O -> not (f O);
						    S x -> not (f(S x)) && ninguno f x}									


						  
-- para las pruebas

par :: N -> Bool
par = \n -> case n of{O -> True; S k -> not (par k)}							  

positivo :: N -> Bool
positivo = \n -> case n of {O -> False ; S x -> True}
	

