{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module LabN where

data N where {O :: N; S :: N -> N}
  deriving (Show)

instance Eq N where 
    (==) = \m n -> case m of {O -> case n of {O -> True ; _-> False};
							S x -> case n of {O -> False ; S y -> x == y}}
instance Ord N where 
	(<=) = \n m -> case n of{
				O -> True;
				S x -> case m of {O -> False ; S y -> x <= y}} 			

instance Num N where
	(+) = \m n -> case m of {O -> n; S x -> S(x + n)}
	(*) = \m n -> case m of {O -> O; S x -> n + (x * n)}
	(-) = \m n -> case m of {O -> O ; S x -> case n of {O -> S x; S y -> x - y}}

--3) funcion potencia
(%)::N -> N -> N
(%) = \m n -> case n of {O -> uno ; S x -> m * (m%x)}

--4) funcion doble
doble :: N -> N
doble = (* dos)
			
--5)  n! = 1 * 2 * 3 * .....* n
fact :: N -> N
fact = \n -> case n of {O -> S O ; S x ->  S x * fact x  }

--6) sumi n = 0 + 1 + 2 + ..... + n
sumi :: N -> N
sumi = \ n -> case n of {O -> O  ; S x -> S x + sumi x }

--7) sumdobles
sumdobles :: N -> N
sumdobles = \ n -> case n of {O -> O  ; S x -> doble(S x) + sumdobles x }

--8) sumfacts n = 0! + 1! + 2! + .... + n!
sumfacts :: N -> N
sumfacts = \ n -> case n of {O -> uno  ; S x -> fact (S x) + sumfacts x }

--9) sumfi f n = f 0 + f 1 + f 2 + .... + f n
sumfi :: (N->N) -> N -> N
sumfi = \f n -> case n of {O -> f O ; S x -> f (S x) + sumfi f x}

--10)

--11) suma pares
sumpares :: N -> N
sumpares = \ n -> case n of {O -> O ; S x -> case par (S x) of {True -> S x + sumpares x;
																False -> sumpares x}}

--12) suma impares
sumimpares :: N -> N
sumimpares = \ n -> case n of {O -> O ; S x -> case par (S x) of {False -> S x + sumimpares x;
																  True -> sumimpares x}}

--13) sumpi

--14) reescribir suma pares y suma impares con sumpi

--15) suma cuadrados impares menores: sumacuadimp

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

impar :: N -> Bool
impar = \n -> not (par n)							  

positivo :: N -> Bool
positivo = \n -> case n of {O -> False ; S x -> True}