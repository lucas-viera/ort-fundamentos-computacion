{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}


module Racionales where
import Naturales

data Q where { R :: N -> N -> Q } deriving Show

num :: Q -> N
num = \(R numerador denominador) -> numerador

den :: Q -> N
den = \(R numerador denominador) -> denominador

instance Eq Q where
    (==) = \(R n1 d1) -> \(R n2 d2) -> n1 * d2 == n2 * d1

instance Ord Q where
    (<=) = \(R n1 d1) -> \(R n2 d2) -> n1 * d2 <= n2 * d1

instance Num Q where
    (+) = \(R n1 d1) -> \(R n2 d2) -> R (n1 * d2 + n2 * d1) (d1 * d2)
    (*) = \(R n1 d1) -> \(R n2 d2) -> R (n1 * n2) (d1 * d2)
    (-) = \(R n1 d1) -> \(R n2 d2) -> R (n1 * d2 - n2 * d1) (d1 * d2)  
	
sumFracc :: N -> Q
sumFracc = \n -> case n of { 
                            O -> (R O uno); 
                            S x -> (R uno (S x)) + (sumFracc x)
                           }

-- Datos de pruebas

unmedio :: Q
unmedio = R uno dos

untercio :: Q
untercio = R uno tres

tresnovenos :: Q
tresnovenos = R tres nueve

uncuarto :: Q
uncuarto = R uno cuatro

dosoctavos :: Q
dosoctavos = R dos ocho

cincotercios :: Q
cincotercios = R cinco tres

tresmedios :: Q
tresmedios = R tres dos

-- Pruebas funciones

pruebanumerador1 = num unmedio
pruebanumerador2 = num cincotercios
pruebanumerador3 = num tresmedios

pruebadenominador1 = den untercio
pruebadenominador2 = den tresmedios
pruebadenominador3 = den uncuarto

pruebaigualdad1 = untercio == tresnovenos
pruebaigualdad2 = unmedio == dosoctavos
pruebaigualdad3 = uncuarto == dosoctavos

pruebamenorigual1 = untercio <= unmedio 
pruebamenorigual2 = unmedio <= uncuarto
pruebamenorigual3 = cincotercios <= unmedio
pruebamenorigual4 = cincotercios <= cincotercios

pruebamayorigual1 = untercio >= unmedio 
pruebamayorigual2 = unmedio >= uncuarto
pruebamayorigual3 = cincotercios >= unmedio
pruebamayorigual4 = cincotercios >= cincotercios

pruebaSumFracc1 = sumFracc O
pruebaSumFracc2 = sumFracc uno
pruebaSumFracc3 = sumFracc dos
pruebaSumFracc4 = sumFracc tres
pruebaSumFracc5 = sumFracc cuatro
