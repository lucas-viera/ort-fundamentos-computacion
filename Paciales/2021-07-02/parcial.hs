{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module PARCIAL where 

import Prelude

--problema 3

data Ta where { H :: Ta;                    -- hojas
                U :: a -> Ta -> Ta ;        -- unaria
                B :: a -> Ta -> Ta -> Ta }  -- binario

--A) paso a paso
--  B 7 () ()
--  B 7 () (U 2 H)              lado izquierda
--  B 7 (U 3 ()) (U 2 H)        primer etapa lado derecho
--  B 7 (U 3 (B 1 H H)) (U 2 H) profundizamos en rama izquierda

-- agregamos el tipo

--  B 7 (U 3 (B 1 H H)) (U 2 H) :: T int

--B) programar funcion bs :: Ta -> N que devuelve
-- la cantidad de nodos binarios

bs :: Ta -> N
bs = \t -> case t of { H -> 0;                         --caso hoja sola, no hay b
                       U x t1 -> bs t1   ;             --caso arbol unario
                       B y t1 t2 -> S(bs t1 + bs t2)   --aca tambien se puede usar x en vez de y
                     }  

--C) programar la funcion abin :: Ta -> Ta

aBin :: Ta -> Ta
aBin = \t -> case t of { H -> H;                        -- una hoja sola
                         U x t1 -> B x (aBin t1)(aBin t1);  --construye un binario compuesto de t1 y t1 para cada lado y recursivamente
                         B y t1 t2 -> B y (aBin t1)(aBin t2)
                       }

--D) demostrar (∀t::T a) bs t <= bs (aBin t)
{--
Dem. Por inducción en t:: T a

Caso t = H (caso base): bs H ≤? bs(aBin H)
	bs H 
	= (def bs)
	0	
	≤ (L1)
	bs(aBin H)


Caso t = U x t1, con x:: a y t1:: T a cualesquiera 
(caso inductivo: 1 sola hipótesis para t1)

HI) bs t1 ≤ bs(aBin t1)
TI) bs (U x t1) ≤? bs(aBin (U x t1))			reemplaza en HI> t = U x t1
	bs (U x t1) 
	    = (def bs)
	bs t1
    	≤ (HI)
	bs(aBin t1)					-- m
    	≤ (L4)
	bs(aBin t1) + bs(aBin t1)
		≤ (L3)
	S(bs(aBin t1)+bs(aBin t1))  -- S(m+m)
		= (def bs)
	bs (B x(aBin t1)(aBin t1))
		= (def. aBin)
	bs(aBin (U x t1))
		Acá usamos L7 (transitividad de ≤)


Caso t = B y t1 t2, con y::a y t1,t2::T a cualesquiera
(caso inductivo con hipótesis para t1 y t2)

HI1) bs t1 ≤ bs(aBin t1)
HI2) bs t2 ≤ bs(aBin t2)
TI) bs(B y t1 t2) ≤? bs(aBin (B y t1 t2))		reemplaza en HI1 y HI2> t = B y t1 t2
	bs(B y t1 t2) 
		= (def. bs)
	S(bs t1 + bs t2)
	
		≤ (L5 con (L6 con HI1 y HI2)) -- por L6 con HI1 y HI2 sabemos que 
			bs t1 + bs t2 ≤ bs(aBin t1)+bs(aBin t2)
									  -- Luego, por L5 tenemos que
			S(bs t1 + bs t2) ≤ S(bs(aBin t1)+bs(aBin t2))
	
	S(bs(aBin t1)+bs(aBin t2))
		= (def bs)
	bs(B y (aBin t1)(aBin t2))
		= (def aBin)
	bs(aBin (B y t1 t2))
--}