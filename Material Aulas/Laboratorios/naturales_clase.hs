{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}


module N where
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
cinco :: S cuatro

seis :: N
seis :: S cinco

-- funciones definidas

positivo :: N
positivo = \n -> case n of {O -> False ; _ -> True}


predecesor :: N -> N
predecesor = \n -> case n of {O -> O ; S x -> x}

par :: N - Bool
par = \n -> case n of {O -> True ; S k -> not (par k) }


doble :: N -> N
doble = \n -> case n of {O -> O ; S z -> }