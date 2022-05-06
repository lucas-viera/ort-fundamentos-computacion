{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Entregable3 where

data N where {O :: N; S :: N -> N}
  deriving (Show)

-- Ej 1
distancia :: N -> N -> N
distancia = undefined

-- Ej 2						
escalera :: N -> N -> N -> Bool
escalera = undefined

-- Ej 3				
dosIguales :: (N -> Bool) -> N -> N
dosIguales = undefined

instance Eq N where
	(==) = \n m -> case n of{
				O -> case m of {O -> True ; S x -> False};
				S x -> case m of {O -> False ; S y -> x == y }}

-- Ej 4
ninguno:: (N -> Bool) -> N -> Bool
ninguno = undefined

						  
-- Para las pruebas

uno:: N
uno = S O

dos :: N
dos = S uno
	
tres :: N
tres = S dos

cuatro :: N
cuatro = S tres

cinco :: N
cinco = S cuatro

par :: N -> Bool
par = \n -> case n of{O -> True; S k -> not (par k)}							  

positivo :: N -> Bool
positivo = \n -> case n of {O -> False ; S x -> True}
	
instance Ord N where
	(<=) = \n m -> case n of{
				O -> True;
				S x -> case m of {O -> False ; S y -> x <= y}} 	



