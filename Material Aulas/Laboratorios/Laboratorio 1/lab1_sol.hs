{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Lab1 where
import Prelude (Show)

data Bool where {False::Bool ; True::Bool}
  deriving Show

not :: Bool -> Bool
not = \x -> case x of {False -> True ; True -> False}


-- Ej2
--Conjuncion (y):
(&&) :: Bool -> Bool -> Bool
(&&) = \x -> \y -> case x of {False -> False ; True -> y}

--Disyunci´on (o inclusivo):
(||) :: Bool -> Bool -> Bool
(||) =  \x -> \y -> case x of { False -> y ; True -> True}


-- O exclusivo (s´olo un argumento es True):
xor :: Bool -> Bool -> Bool
xor = \x -> \y -> case x of { False -> y ; True -> not y }

-- Ni (da True cuando ambos argumentos son False):
ni :: Bool -> Bool -> Bool
ni = \x -> \y -> case x of {False -> not y ; True -> False}


--EJ3

--Definir usando case
(==) :: Bool -> Bool -> Bool 
(==) = \x -> \y -> case x of {False -> not y ; True -> y}


--Dar otra definici´on de esta funci´on usando otras funciones ya definidas
-- y sin usar case 
--(va a ser necesario darle otro nombre a la nueva funci´on, por ejmplo ===).

(===) :: Bool -> Bool -> Bool 
(===) = \x -> \y -> not (xor x y)

(====) :: Bool -> Bool -> Bool 
(====) = \x -> \y -> (x&&y) || (ni x y)



--Definir la desigualdad booleana (/=) :: Bool -> Bool -> Bool.
--Compararla con el conectivo xor, el o exclusivo. ¿Qu´e conclusi´on se puede sacar?.

(/=) :: Bool -> Bool -> Bool
(/=) = xor




--Defina el orden entre booleanos como una funci´on (≤) :: Bool -> Bool -> Bool, 
--de modo tal que False sea menor que True.

(<=):: Bool -> Bool -> Bool
(<=) = \x -> \y -> case x of {False -> True ; True -> y}



--Ej4 

--Programar las siguientes funciones booleanas. 
--En todos los casos dar dos definiciones:
--una usando case y otra usando los conectivos definidos en los ejercicios anteriores 
--(ser´a necesario ponerle nombres diferentes a las funciones cuando se definan por segunda vez):


unanimidad :: Bool -> Bool -> Bool -> Bool
--recibe tres booleanos y devuelve verdadero cuando los tres booleanos recibidos sean iguales.
unanimidad = \x y z -> case x of {True -> case y of {True -> z ; False->False};
								 False -> case y of {True -> False ; False -> not z}}

u1 :: Bool -> Bool -> Bool -> Bool
u1 =  \x y z -> case x of {True -> y&&z ; False -> ni y z}

u2 :: Bool -> Bool -> Bool -> Bool
u2 =  \x y z -> x&&y&&z  ||  ni x y && not z  

u3 :: Bool -> Bool -> Bool -> Bool
u3 =  \x y z -> x == y && y == z


mayoria :: Bool -> Bool -> Bool -> Bool
--recibe tres booleanos y devuelve el resultado de la mayor´ıa de ellos.
mayoria = \x y z -> case x of {True -> case y of {True -> True  ; False-> z};
							  False -> case y of {True -> z  ; False -> False }}

m1 :: :: Bool -> Bool -> Bool -> Bool
m1 = \x y z -> case x of {True -> y ||z;
						  False -> y&&z}

m2 :: :: Bool -> Bool -> Bool -> Bool
m2 = \x y z -> x&&y  || x&&z || y&&z
						  
m3 :: Bool -> Bool -> Bool -> Bool
m3 = \x -> \y -> \z -> (x && (y || z)) || (y && (x || z))
						  
						  
impar :: Bool -> Bool -> Bool -> Bool 
--impar b1 b2 b3 es True cuando una cantidad impar de sus argumentos es True.
impar = \x y z -> case x of {True -> y == z;
							False -> y /= z}

i2  :: Bool -> Bool -> Bool -> Bool