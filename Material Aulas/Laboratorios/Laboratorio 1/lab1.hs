{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Lab1 where
import Prelude (Show)

data Bool where {False:Bool ; True::Bool}
    deriving Show

not :: Bool -> Bool
not = \x -> case x of {False -> True}