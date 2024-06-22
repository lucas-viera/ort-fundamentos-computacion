Funcion `null` devuelve `True` si la lista es vacia y False si la lista tiene al menos un elemento.

```haskell
null :: [a] -> Bool
null = \l -> case l of {
                          [] -> True;
                          x:xs -> False
                       }
```

Funcion `sum` devuelve la suma de los elementos de una lista de enteros.

```haskell
sum :: [N] -> N
sum = \l -> case l of { 
                        [] -> 0; 
                        x:xs -> x + sum xs
                      }
```

Funcion `length` devuelve la cantidad de elementos dentro de la lista.

```haskel
length :: [a] -> N
length = \l -> case l of {
                            [] -> 0;
                            x:xs -> S(length xs)
                          }
```

Funcion `elem` devuelve si un elemento pertenece o no a una lista.

Se puede utilizar `==`, `!=`, `<=`, `>=` etc

```haskell
elem :: Eq a => a -> [a] -> Bool
elem = \e -> \l -> case l of {
                          [] -> False;
                          x:xs -> case (x == e) {
                                                False -> elem e xs;
                                                True -> True
                                              }
                        }
```

Funcion `map` devuelve una lista con una funcion aplicada a todos los elementos de la lista

```haskell
map :: (a->b) -> [a] -> [b]
map = \f -> \l -> case l of {
                              [] -> [];
                              x:xs -> (f x) : (map f xs)
                            }
```

Funcion `filter` devuelve una lista de elementos que cumplen con el predicado

```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter = \p -> \l -> case l of {
                                [] -> [];
                                x:xs -> case (p x) {
                                                    False -> filter p xs
                                                    True -> x : filter p xs
                                                  }
                                } 
```