--Clase 19 - 19 de mayo
{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Listas where
--importa preludio pero sin las funciones dentro de hiding
import Prelude hiding (null,length,sum,map,zip,zipWith,filter,and,or,any,all,(++),reverse,concat,head,tail,last,init,(!!),fst,snd,take,drop,takeWhile,dropWhile,split,elem)

--E1.1
null :: [a] -> Bool
null = \ l -> case l of {[] -> True; 
                          _ -> False}

--E1.2
length :: [a] -> N
length = \ l -> case l of {[] -> O; 
                         y:ys -> S(length ys)}

--E1.3
duplicate :: [a] -> [a]
duplicate = \ l -> case l of {[] -> []; 
                            y:ys -> y:y:duplicate ys}

--E1.4
sum :: [N] -> N
sum = \ l -> case l of {[] -> O; y:ys -> y + sum ys}

--E1.5
prod :: [N] -> N
prod = \ l -> case l of {[] -> S O; y:ys -> y * prod ys}  -- similar a factorial?

--E1.6
map :: (a -> b) -> [a] -> [b]       --a y b son de cualquier tipo por estar en minuscula
map = \ f l -> case l of {[] ->[]; y:ys -> f y:map f ys}    --aplica f al primer elemento (f y) luego replica map pero con f (identica) ys (que es la lista sin el primer elemento)

--E1.7
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = \ f l1 l2 -> case l1 of {[] -> [];
                                 x:xs -> case l2 of {[] -> [];
                                                   y:ys -> f x y:zipWith f xs ys}}
                                                        --aplica f a x e y
                                                        --luego llama zipWith de xs e ys q
                                                        -- que son listas 'recortadas'
--E1.8
filter :: (a -> Bool) -> [a] -> [a]
filter = \ p l -> case l of {[] -> [];
                           x:xs -> case p x of {False -> filter p xs;           --OBS: case p x of
                                                True -> x: filter p xs}}       
                                -- casos True, devuelve el elemento Y ademas la llamada recursiva

--E1.9
and :: [Bool] -> Bool
and = \ l -> case l of {[] -> True; y:ys -> y && and ys}

--E1.10
or :: [Bool] -> Bool
or = \ l -> case l of {[] -> False; y:ys -> y || or ys}

--E1.11
count :: (a -> Bool) -> [a] -> N
count = \p l -> case l of {[] -> O;
                           x:xs -> case p x of {False -> count p xs;       --OBS: case p x of. False, no suma
                                                 True -> S(count p xs) }}   -- casos True suma

--E1.12
any :: (a -> Bool) -> [a] -> Bool
any = \ p l -> case l of {[] -> False;
                        x:xs -> p x || any p xs}    --devuelve p x OR p xs cualquiera sea true antes
        
--E1.13
all :: (a -> Bool) -> [a] -> Bool
all = \ p l -> case l of {[] -> True;   -- devuelve True con lista vacia
                         x:xs -> p x && all p xs} --devuelve p x AND p xs recursiva

--E1.14 redefinir sin case: all, any y count
count1 :: (a -> Bool) -> [a] -> N
count1 = \ p l -> length (filter p l); --filtrar por propiedad y mide largo

count2 :: (a -> Bool) -> [a] -> N
count2 = \ p -> length . (filter p) --otra forma con composicion de funciones

any1 :: (a -> Bool) -> [a] -> Bool
any1 = \ p l -> length (filter p l) >= S O; --similar a count pero comparando con >= 1

any2 :: (a -> Bool) -> [a] -> Bool
any2 = \p l -> or (map p l)

all1 :: (a -> Bool) -> [a] -> Bool
all1 = \ p l -> length (filter p l) == length l; --simular pero comparando con el largo original

--E1.15 
(++) :: [a] -> [a] -> [a]
(++) = \l1 l2 -> case l1 of {[] -> l2;              --concatenar vacio + algo = algo solamente
                           x:xs -> x:(xs ++ l2)}    -- concatenacion recursiva

--E1.16 
reverse :: [a] -> [a]
reverse = \l -> case l of {[] -> []; 
                         x:xs -> reverse xs ++ [x]} 
                                -- se apoya en la concatenacion

--E1.17
elem :: Eq a => a-> [a]-> Bool
elem = \a l -> case l of {[] -> False;
				        x:xs -> (a==x)||(elem a xs)}        -- devuelve la comparacion o la recursion
                        --tiene que estar definido el ==
--E1.18
ultimo::(a -> Bool) -> [a] -> N
ultimo = \p l -> case l of {[] -> O;                       --OBS case p xs of
							x:xs -> case p x of {True -> case ultimo p xs of {O -> S O;              --devuelve el primer lugar
                                                                              _ -> S(ultimo p xs)};  --la posicion cumple + 1
												False -> case ultimo p xs of {O -> O;                --caso False, ninguno cumple
                                                                              _ -> S(ultimo p xs)}}} --la posicion que cumple + 1

--E1.19
concat :: [[a]] -> [a]      --'lambda eles'
concat = \ls -> case ls of {[] -> []  ; 
                            xs:xss -> xs ++ concat xss}   --devuelve el anexo de xs a xss
                            --xss = lista de listaS
--E1.20
lensum :: [[a]] -> N
lensum = undefined


---------------------------------------------------------------------------
--Parte 2 pares ordenados

fst :: (a,b) -> a
fst = \p -> case p of {(x,y) -> x}

snd :: (a,b) -> b
snd = \p -> case p of {(x,y) -> y}

zip :: [a] -> [b] -> [(a,b)]
zip = \l1 l2 -> case l1 of {[] -> [] ; x:xs -> case l2 of {[] -> [] ; y:ys -> (x,y) : zip xs ys }}

--------------------------------------------------------
-- error :: String -> a

head :: [a] -> a
head = \l -> case l of {[] -> error "head: lista vacía" ; x:xs -> x}

pre :: Bool -> String -> a -> a
pre = \b m p -> case b of {False -> error m ; True -> p}

tail :: [a] -> [a]
tail = \l -> pre (not(null l)) "tail: lista vacía" (case l of {x:xs -> xs})


last :: [a] -> a
last = \l -> pre  (not(null l)) "last: lista vacía" (head(reverse l))

init :: [a] -> [a]
init = \l -> pre  (not(null l)) "last: lista vacía" (reverse(tail(reverse l)))













------------------------------------------------------
data N where { O :: N ; S :: N -> N } deriving Show

-- Para las pruebas

uno:: N
uno = S O

dos :: N
dos = S uno
	
tres :: N
tres = S dos

cuatro :: N
cuatro = S tres

cinco :: N
cinco = S cuatro

instance Num N where
    (+) = \m n -> case m of {
        O -> n;
        S x -> S(x + n)
    };
    (*) = \m n -> case m of {
        O -> O;
        S x -> n + x * n
    };
    (-) = \m n -> case m of {
        O -> O;
        S x -> case n of {
            O -> S x;
            S y -> x - y
        }
    };

instance Eq N where
    (==) = \m n -> case m of {
        O -> case n of {
            O -> True;
            _ -> False;
        };
        S x -> case n of {
            O -> False;
            S y -> x == y;
        }
    }

instance Ord N where
    (<=) = \m n -> case m of {
        O -> True;
        S x -> case n of {
            O -> False;
            S y -> x <= y
        }
    }