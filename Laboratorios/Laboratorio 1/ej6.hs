{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
import Data.Complex (imagPart)
data Bool where {False::Bool ; True::Bool}
  deriving Show

impar :: Bool -> Bool -> Bool -> Bool
impar == \x -> \y -> \z -> (x && ni y z) || (y && ni x z) || (z && ni y x) || (x&&y&&z)