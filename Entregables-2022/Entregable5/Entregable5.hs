{-# OPTIONS_GHC -fno-warn-tabs #-}

module Entregable5 where
import Prelude hiding (pert)
type Conj a = [a]

-- Ej 1
pert :: Eq a => a -> Conj a -> Bool
pert = \elem con -> case con of {[] -> False; 
                                 x:xs -> case x==elem of {True -> True;
                                                          False ->pert elem xs}}
-- Ej 2						
esConj :: Eq a => [a] -> Bool
esConj = \lis -> case lis of {[] -> True;
                              x:xs -> case pert x xs of {True -> False;
                                                         False -> esConj xs}}

-- Ej 3				
inter :: Eq a => Conj a -> Conj a -> Conj a
inter = \c1 c2 -> case c1 of {[] -> [];
                              x:xs -> case c2 of {[] -> [];
                                                  y:ys -> case pert x c2 of {True -> x:inter xs c2;
                                                                             False -> inter xs c2}}}

-- Ej 4
union :: Eq a => Conj a -> Conj a -> Conj a
union = \c1 c2 -> case c1 of {[] -> c2;
                              x:xs -> case c2 of {[] -> c1;
                                                  y:ys -> case pert x c2 of {False -> x:c2;
                                                                             True -> union xs c2}}}

-- Ej 5
incl :: Eq a => Conj a -> Conj a -> Bool
incl = \c1 c2 -> case c1 of {[] -> True;
                             x:xs -> case pert x c2 of {True -> incl xs c2;
                                                        False -> False}}

-- Ej 6
iguales :: Eq a => Conj a -> Conj a -> Bool
iguales = \c1 c2 ->  incl c1 c2 && incl c2 c1


-- Para las pruebas

prim :: Conj Int
prim = [11,2,3,7,5]

pares :: Conj Int
pares = [2,4,6,8,10]

impares :: Conj Int
impares = [1,3,5,7,9]

vocales :: Conj Char
vocales = "aeiou"

abc :: Conj Char
abc = "ioufedcba"
