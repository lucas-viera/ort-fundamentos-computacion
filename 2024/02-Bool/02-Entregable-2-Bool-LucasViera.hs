{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use lambda-case" #-}



module Entregable2 where
  import Prelude (Show)
  data Bool where {False :: Bool; True :: Bool } deriving Show
  
  -- funcion not disponible
  not :: Bool -> Bool
  not = \b -> case b of {
                    False -> True;
                    True -> False
  }
  
  -- funcion distintos devuelve True cuando los 3 Bool no son iguales entre si
  distintos :: Bool -> Bool -> Bool -> Bool
  distintos = \b1 -> \b2 -> \b3 -> case b1 of {
                                      False -> case b2 of {
                                                    False -> b3;
                                                    True -> True
                                      };
                                      True -> case b2 of {
                                                    False -> True;
                                                    True -> not b3
                                      };
  }