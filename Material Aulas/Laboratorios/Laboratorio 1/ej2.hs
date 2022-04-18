{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

-- (&&) conjuncion
(&&) :: Bool -> Bool -> Bool
(&&) = \x -> \y -> case x of {False -> False ; True -> y}

-- (||) disyuncion
(||) :: Bool -> Bool -> Bool
(||) = \x -> \y -> case x of {True -> True ; False -> y }

-- (xor) o excluyente
xor :: Bool -> Bool -> Bool
xor = \x -> \y -> case x of {False -> y ; True -> not y}

-- (ni) nor (da true cueando ambos argumentos son falsos)
ni :: Bool -> Bool -> Bool
ni = \x -> \y -> case x of {False -> not y; True -> False}

-- XOR y NI no son operadores infijos, 
-- porque no estan entre parentesis
-- entonces se pone 'ni True False' (va adelante)
-- y para xor 'xor True True' por ejemplo

-- si lo quisiera poner en medio tiene que
--  ir entre ` `

