{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}

module Lab1 where
  import Prelude (Show)
  data Bool where {False::Bool ; True::Bool} deriving Show

-- Ejercicio 1: NOT
  not :: Bool -> Bool
  not = \b -> case b of {
                    False -> True;
                    True -> False
  }

-- Ejercicio 2: OR, AND, XOR, Conectivo
  (||) :: Bool -> Bool -> Bool
  (||) = \b1 -> \b2 -> case b1 of {
                          True -> True;
                          False -> b2
  }
  
  (&&) :: Bool -> Bool -> Bool
  (&&) = \b1 -> \b2 -> case b1 of {
                          False -> False;
                          True -> b2
  }
  
  xor :: Bool -> Bool -> Bool
  xor = \b1 -> \b2 -> case b1 of {
                            True -> case b2 of {
                                          True -> False;
                                          False -> True
                            };
                            False -> case b2 of {
                                          True -> True;
                                          False -> False
                            }
  }
  
  (>>) :: Bool -> Bool -> Bool
  (>>) = \b1 -> \b2 -> case b1 of {
                            True -> case b2 of {
                                          True -> True;
                                          False -> False
                            };
                            False -> case b2 of {
                                          True -> True;
                                          False -> True
                            }
  }

-- Ejercicio 3: equivalencia logica <=> , desigualdad (/=) y orden (<=)
  (==) :: Bool -> Bool -> Bool
  (==) = \b1 -> \b2 -> case b1 of {
                            True -> case b2 of {
                                        True -> True;
                                        False -> False
                            };
                            False -> case b2 of {
                                        True -> False;
                                        False -> True
                            }
  }
  -- Otra forma 1
  (===) :: Bool -> Bool -> Bool
  (===) = \b1 -> \b2 -> not (xor b1 b2)
  
  --Otra forma 2
  (====) :: Bool -> Bool -> Bool
  (====) = \b1 -> \b2 -> (not b1) && (not b2) || (b1 && b2)
  
  
  (/=) :: Bool -> Bool -> Bool
  (/=) = \b1 -> \b2 -> xor b1 b2

  (<=) :: Bool -> Bool -> Bool
  (<=) = \b1 -> \b2 -> case b1 of {
                            False -> True;
                            True -> b2
  }


-- Ejercicio 4:
  unanimidad :: Bool -> Bool -> Bool -> Bool
  unanimidad = \b1 -> \b2 -> \b3 -> case b1 of {
                                            True -> case b2 of {
                                                          True -> case b3 of {
                                                                        True -> True;
                                                                        False -> False
                                                          };
                                                          False -> False
                                            };
                                            False -> case b2 of {
                                                          False -> case b3 of {
                                                                        True -> False;
                                                                        False -> True
                                                          };
                                                          True -> False
                                            }
  }
  
  mayoria :: Bool -> Bool -> Bool -> Bool
  mayoria = \b1 -> \b2 -> \b3 -> case b1 of {
                                      True -> case b2 of {
                                                    True -> True;                 -- T T
                                                    False -> case b3 of {
                                                                  True -> True;   -- T F T
                                                                  False -> False  -- T F F
                                                    };
                                      };
                                      False -> case b2 of {
                                                    True -> case b3 of {
                                                                  True -> True;   -- F T T
                                                                  False -> False  -- F F F
                                                    };
                                                    False -> False                -- F F
                                      };
  }
  
  mayoriaMejorado :: Bool -> Bool -> Bool -> Bool
  mayoriaMejorado = \b1 -> \b2 -> \b3 -> case b1 of {
                                              True -> case b2 of {
                                                            True -> True;
                                                            False -> b3
                                              };
                                              False -> case b2 of {
                                                            True -> b3;
                                                            False -> False
                                              };
  }
  
  -- devuelve True cuando una cantidad impar es True
  impar :: Bool -> Bool -> Bool -> Bool
  impar = \b1 -> \b2 -> \b3 -> case b1 of {
                                    True -> case b2 of {
                                                  True -> b3;
                                                  False -> not b3
                                    };
                                    False -> case b2 of {
                                                  True -> not b3;
                                                  False -> b3
                                    }
  }


-- Ejercicio 5: redefinir (&&) y (>>) sin case. Solo not y (||)    
  
  -- AND (x,y) = NOT( NOT(x) OR NOT(y) )
  (&&&) :: Bool -> Bool -> Bool
  (&&&) = \b1 -> \b2 -> not ((not b1) || (not b2))

  -- XOR (x,y) = (x AND NOT y) OR (NOT x AND y)
  xorr :: Bool -> Bool -> Bool
  xorr = \b1 -> \b2 -> (b1 && not b2) || (not b1 && b2)


-- Ejercicio 6:
  -- funcion (@@) es True cuando alguno de los argumentos es False
  (@@) :: Bool -> Bool -> Bool
  (@@) = \b1 -> \b2 -> case b1 of {
                            False -> True;
                            True -> not b2
  }
  --funcion (#) es True cuando solo uno de los argumentos es False
  (#) :: Bool -> Bool -> Bool
  (#) = \b1 -> \b2 -> case b1 of {
                          True -> not b2;
                          False -> b2
  }
  
  --funcion tri devuelve True si hay mas False que True
  tri :: Bool -> Bool -> Bool -> Bool
  tri = \b1 -> \b2 -> \b3 -> case b1 of {
                                  True -> case b2 of {
                                              True -> False;
                                              False -> not b3
                                  };
                                  False -> case b2 of {
                                              False -> True;
                                              True -> not b3
                                  };
  }
  
