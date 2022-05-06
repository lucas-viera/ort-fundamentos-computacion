{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Naturales where

data N where {O :: N ; S :: N -> N} deriving Show

-- definiciones auxiliares
uno :: N
uno = S O

dos :: N
dos = S uno

tres :: N
tres = S dos

cuatro :: N
cuatro = S tres

cinco :: N
cinco = S cuatro

seis :: N
seis = S cinco

-- funciones definidas

predecesor :: N -> N
predecesor = \n -> case n of {O -> O ; S x -> x}

-- Clases en HASKEL
instance Eq

