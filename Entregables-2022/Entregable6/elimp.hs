{-# OPTIONS_GHC -fno-warn-tabs #-}

module Entregable6 where
import Prelude hiding (pert)
type Conj a = [a]

elimp :: (a -> Bool) -> [a] -> [a]
elimp = \p l -> case l of { [] -> []; 
                            x:xs -> case p x of { True -> elimp p xs; 
                                               False -> x : elimp p xs}}


