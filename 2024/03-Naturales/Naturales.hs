{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Naturales where
  data N where { O :: N ; S :: N -> N} deriving Show
  
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

  siete :: N
  siete = S seis

  ocho :: N
  ocho = S siete

  nueve :: N
  nueve = S ocho

  predecesor :: N -> N
  predecesor = \n -> case n of {O -> O; S x -> x}
  
  par :: N -> Bool 
  par = \n -> case n of {O -> True; S k -> not (par k)} 

  impar :: N -> Bool 
  impar = \n -> not (par n)                              

  positivo :: N -> Bool 
  positivo = \n -> case n of {O -> False ; S x -> True} 
  
  -- Ejercicio 1
  instance Eq N where
    (==) = \m -> \n -> case m of {
                                    O -> case n of {O -> True ; _ -> False};
                                    S x -> case n of {O -> False ; S y -> x == y};
                                  }
  -- Ejercicio 2
  instance Ord N where
    (<=) = \m -> \n -> case m of {
                                  O -> True ;
                                  S x -> case n of {O -> False ; S y -> x <= y} 
                                  } 
    (>) = \m -> \n -> not (m <= n)
    (>=) = \m -> \n -> (m > n) || (m == n)
    
    (<) = \m -> \n -> not (m >= n)
      
  

  minimo :: N -> N -> N
  minimo = \m -> \n -> case m < n of {True -> m ; False -> n}

  maximo :: N -> N -> N
  maximo = \m -> \n -> case m > n of { True -> m; False -> n }
                                      
  min3 :: N -> N -> N -> N
  min3 = \m -> \n -> \o -> minimo (minimo m n) (minimo n o)
  
  
  
  -- Ejercicio 3
  instance Num N where
    (+) = \m -> \n -> case m of {O -> n; S x -> S(x + n)}

    (*) = \m -> \n -> case m of {O -> O; S x -> n + (x * n)}

    (-) :: N -> N -> N
    (-) = \m -> \n -> case m of {
                                  O -> O ; 
                                  S x -> case n of {
                                                      O -> S x; 
                                                      S y -> x - y
                                                   }
                                }
  -- potencia (%) por fuera de instance Num
  (%) :: N -> N -> N
  (%) = \m -> \n -> case n of {O -> uno; S x -> m * (m%x)}
                                
  doble :: N -> N
  doble = \m -> m * dos
  
  fact :: N -> N
  fact = \m -> case m of {O -> uno ; S x -> S x * fact x}
  
  sumi :: N -> N
  sumi = \m -> case m of { O -> O ; S x -> S x + sumi x}
    
  sumdobles :: N -> N
  sumdobles = \m -> case m of { O -> O ; S x -> doble (S x) + sumdobles x}
    
  sumfacts :: N -> N
  sumfacts = \m -> case m of { O -> uno ; S x -> fact(S x) + sumfacts x}
  
  -- recibe una funcion y un entero y devuelve la suma de la aplicacion de la funcion a ese entero
  sumfi :: (N -> N) -> N -> N 
  sumfi = \f -> \n -> case n of { O -> f O ; S x -> f (S x) + sumfi f x}
    
  sumpares :: N -> N
  sumpares = \m -> case m of {
                              O -> O;
                              S x -> case par (S x) of {
                                                        False -> sumpares x ;
                                                        True -> S x + sumpares x
                                                        }
                              }
  sumimpares :: N -> N
  sumimpares = \m -> case m of {
                              O -> O;
                              S x -> case impar (S x) of {
                                                        False -> sumimpares x ;
                                                        True -> S x + sumimpares x
                                                        }
                              }
  
  sumpi :: (N -> Bool) -> N -> N
  sumpi = \f -> \n -> case n of {
                                  O -> O;
                                  S x -> case f (S x) of {False -> sumpi f x; True -> S x + sumpi f x}
  }
  
  sumparespi :: N -> N
  sumparespi = \m -> sumpi par m
  
  sumimparespi :: N -> N
  sumimparespi = \m -> sumpi impar m
  
  cuadrado :: N -> N
  cuadrado = \m -> m % dos
  
  sumcuadimp :: N -> N
  sumcuadimp = \m -> sumfi cuadrado m
  --usar sumfi 