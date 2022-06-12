alternancia:: a -> a -> [a] -> Bool
alternancia = \x y l -> case l of {[] -> ;
                                   z:zs -> case z == x of  {False -> False; 
                                                            True -> alternancia y x zs}}

data N where {O::N;S::N->N} deriving Show 
altGen :: N -> a -> a -> [a]
altGen = \n x y -> case n of {O -> [];
                             S z -> x : altGen z y x}