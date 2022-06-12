{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module LabN where

data N where {O :: N; S :: N -> N}
  deriving (Show)



contarp :: (a -> Bool) -> [a] -> N
contarp = \p l -> case l of { [] -> O; 
                            x:xs -> case p x of { True -> S(contarp p xs); 
                                               False -> contarp p xs}}


