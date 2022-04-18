{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Lab1 where
import Prelude (show, Bool)

--a Definir (==) usando case
(==) :: Bool -> Bool -> Bool
(==) = \x -> \y -> case x of {False -> not y; True -> y}

--b Dar otra definicion usando otras funciones
-- ya definidas y sin usar case
-- (xor) o excluyente
xor :: Bool -> Bool -> Bool
xor = \x -> \y -> case x of {False -> y ; True -> not y}

(===) :: Bool -> Bool -> Bool
(===) = \x -> \y -> not (xor x y) --usa xor

ni :: Bool -> Bool -> Bool
ni = \x -> \y -> case x of {False -> not y; True -> False}
(====) :: Bool -> Bool -> Bool
(====) = \x -> \y -> (x&&y) || (ni x y) --usa and y ni

--c desigualdad booleana (/=) 
(/=) :: Bool -> Bool -> Bool
--es igual a XOR tal cual
(/=) = xor


--d Definir el menor o igual para el caso de False < True
(<=) :: Bool -> Bool -> Bool
(<=) = \x -> \y -> case x of {False -> True ; True -> y}