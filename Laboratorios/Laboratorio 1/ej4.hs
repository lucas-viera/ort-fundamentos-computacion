{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Lab1 where
import Prelude (Show)

--Programar las siguientes funciones booleanas dando 2 definiciones:
-- una usando case y otra usando los conectivos definidos en los ejercicios anteriores 

data Bool where {False::Bool ; True::Bool}
  deriving Show

u1 :: Bool -> Bool -> Bool -> Bool
u1 =  \x y z -> case x of {True -> y&&z ; False -> ni y z}

u2 :: Bool -> Bool -> Bool -> Bool
u2 =  \x y z -> x&&y&&z  ||  ni x y && not z  

u3 :: Bool -> Bool -> Bool -> Bool
u3 =  \x y z -> x == y && y == z

