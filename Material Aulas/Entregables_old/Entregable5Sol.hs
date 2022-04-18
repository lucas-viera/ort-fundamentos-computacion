{-# OPTIONS_GHC -fno-warn-tabs #-}

module Entregable5 where

remDups :: Eq a => [a] -> [a]
remDups = \l -> case l of {[] -> []; x:xs -> x : filter (/=x) (remDups xs) }

{-- (∀l::[a]) remDups (remDups  l) = remDups l
Dem. Por inducción en l::[a]
Caso l = []: remDups (remDups []) = remDups []
	remDups (remDups [])
	= (def. remDups)
	remDups []
	
Caso l = x:xs, con x::a, xs::[a] cualesquiera
HI) remDups(remDups xs) = remDups xs
TI) remDups(remDups (x:xs)) = remDups (x:xs)
	remDups(remDups (x:xs))
	= (def. remDups)
	remDups(x : filter (/=x) (remDups xs))
	= (def remDups)
	x : filter (/=x) (remDups (filter (/=x) (remDups xs))
	= (Lemafilter: (∀p::a->Bool) (∀l::[a]) remDups (filter p l) = filter p (remDups l))
	x : filter (/=x) (filter (/=x) (remDups (remDups xs)))
	= (HI)
	x : filter (/=x) (filter (/=x) (remDups xs))
	= (Lemafilter2: (∀p :: a -> Bool)(∀l :: [a]) filter p (filter p l) = filter p l)
	x : (filter (/=x) (remDups xs))
	
	Por otro lado:
	remDups (x:xs)
	= (def. remDups)
	x : filter (/=x) (remDups xs)
	
	Ambas expresiones son iguales por reducción a la misma expresión
	
	
(∀l::[a]) length (remDups l) ≤ length l
Dem. Por inducción en l::[a]
Caso l = []: length (remDups []) ≤ length []
	length (remDups []) 
	= (def. remDups)
	length []
	≤ (Lema≤refl: (∀n :: N) n ≤ n)
	length []

Caso l = x:xs, con x::a, xs::[a] cualesquiera
HI) length (remDups xs) ≤ length xs
TI) length (remDups (x:xs)) ≤ length (x:xs)
	length (remDups (x:xs))
	= (def. remDups)
	length (x : filter (/=x) (remDups xs))
	= (def. length)
	S (length (filter (/=x) (remDups xs)))
	≤ (Lema≤SS: (∀m :: N)(∀n :: N) m ≤ n ⇒ S m ≤ S n
		+ Lemafilter≤: (∀p :: a -> Bool)(∀l :: [a]) length (filter p l) ≤ length l) 
	S (length (remDups xs))
	≤ (Lema≤SS: (∀m :: N)(∀n :: N) m ≤ n ⇒ S m ≤ S n + HI)
	S (length xs)
	= (def. length)
	length (x:xs)
	Se cumple por transitividad de ≤
--}


type Set a = [a]

pertenece :: Eq a => a -> Set a -> Bool
pertenece = \e s -> case s of {[] -> False ; 
							   x:xs -> case e == x of {True -> True ; False -> pertenece e xs}}

incluido:: Eq a => Set a -> Set a -> Bool
incluido = \s t -> case s of {[] -> True ; x:xs -> pertenece x t && incluido xs t}

incluido2:: Eq a => Set a -> Set a -> Bool
incluido2 = \s t -> filter (`pertenece` t) s == s

incluido3:: Eq a => Set a -> Set a -> Bool
incluido3 = \s t -> all (`pertenece` t) s


iguales :: Eq a => Set a -> Set a -> Bool
iguales = \s t -> incluido s t && incluido t s


union:: Eq a => Set a -> Set a -> Set a
union = \s t -> case s of { [] -> t ; 
							x:xs -> case pertenece x t of {True -> union xs t; 
														   False -> x : union xs t}}
														   
union2 :: Eq a => Set a -> Set a -> Set a
union2 = \s t -> remDups (s ++ t)


							
interseccion :: Eq a => Set a -> Set a -> Set a
interseccion = \s t -> case s of { [] -> [] ; 
							x:xs -> case pertenece x t of {True -> x : interseccion xs t; 
														   False -> interseccion xs t}}							
							
interseccion2 :: Eq a => Set a -> Set a -> Set a
interseccion2 = \s t -> filter (`pertenece` s) t							
							
							
							
							
							