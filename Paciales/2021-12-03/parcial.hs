data T a b where {  J :: a -> T a b ;		--Semilla
					K :: b -> T a b ;		--Semilla
					N :: T a b -> T a b -> T a b ;	--Generador
					P :: T a b -> T a b }			--Generador

numH :: T a b -> N
numH = \t -> case t of {J x -> S 0;
						 K y -> S 0;
						 N t1 t2 -> numH t1 + numH t2;
						 P t1 -> numH t1}

numN :: T a b -> N
numN = \t -> case t of { J x -> 0;
						 K y -> 0;
						 N t1 t2 -> S(numN t1 + numN t2);
						 P t1 -> numN t1}

(∀t::T a b) numH t = S(numN t)
Dem. Por inducción en t:: T a b
Caso t = J x, con x::a cualquiera (caso base 1)
numH (J x) =? S(numN (J x))
	


Caso t = K y, con y::b... (caso base 2)
análogo



Caso t = N t1 t2, con T1, t2:: T a b cualesquiera
HI1) numH t1 = S(numN t1)
HI2) numH t2 = S(numN t2)
TI) numH (N t1 t2) = S(numN (N t1 t2))
.....

Caso t = P t1, con t1....
HI)numH t1 = S(numN t1)
TI) numH (P t1) = S(numN (P t1))
.........

