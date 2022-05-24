{-# OPTIONS_GHC -fno-warn-tabs #-}

module Entregable5 where

type Conj a = [a]

-- Ej 1
pert :: Eq a => a -> Conj a -> Bool
pert = undefined

-- Ej 2						
esConj :: Eq a => [a] -> Bool
esConj = undefined

-- Ej 3				
inter :: Eq a => Conj a -> Conj a -> Conj a
inter = undefined

-- Ej 4
union :: Eq a => Conj a -> Conj a -> Conj a
union = undefined

-- Ej 5
incl :: Eq a => Conj a -> Conj a -> Bool
incl = undefined

-- Ej 6
iguales :: Eq a => Conj a -> Conj a -> Bool
iguales = undefined


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
abc = "fedcba"
