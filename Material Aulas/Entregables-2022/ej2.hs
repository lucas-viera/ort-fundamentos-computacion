{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
import Prelude (Show)
data Bool where {False::Bool ; True::Bool}
  deriving Show


(*):: Bool -> Bool -> Bool 
-- (*)= \x -> \y -> case x of {True -> False ; False -> not y}  Pero sin utilizar not
(*) = \x -> \y -> case x of {True -> False ; False -> case y of {True -> False ; False -> False}}
