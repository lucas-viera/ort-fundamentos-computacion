partT :: Bool -> Bool -> Bool -> Bool
part$ = \b1 -> \b2 -> \b3 = case b1 of {
                                        False -> case b2 of {
                                                              False -> not b3;
                                                              True -> b3
                                        };
                                        True -> case b2 of {
                                                              False -> b3;
                                                              True -> not b3
                                                            }
                                          }
                                          
sim :: Bool -> Bool -> Bool -> Bool
sim = = \b1 -> \b2 -> \b3 = case b1 of { False -> not b3; True -> b3 }


consec :: Bool -> (N -> Bool) -> Bool
consec = \n -> \p -> case n of {
                                  0 -> False;
                                  Sx -> case p(Sx) of {
                                                        False -> consec x p;
                                                        True -> case px of {
                                                                            False -> consec x p;
                                                                            True -> True
                                                                            }
                                                      }  
                                }
in :: N -> N -> Bool
in = \m -> \n -> case m of {
                            0 -> True;
                            Sx -> case n of {
                                              0 -> False;
                                              Sy -> in x y
                                            }
                            }
                            
out :: N -> N -> Bool
out = \m -> \n -> case m of {
                            0 -> False;
                            Sx -> case n of {
                                              0 -> True;
                                              Sy -> out x y
                                            }
                            }
                            
                            