{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module FS where 

import Prelude

-- Nombre: Lucas Viera
-- Número: 177863

type Nombre = String 

data Ext where { Txt::Ext ; Mp3::Ext ; Jar::Ext ; Doc::Ext ; Hs::Ext }
  deriving (Eq)

instance Show Ext where 
	show = \ex -> case ex of{ Txt-> "txt" ; 
					     	 Mp3-> "mp3" ; 
						     Jar-> "jar" ; 
						     Doc-> "doc" ; 
						     Hs-> "hs"}

data FS where {  A :: (Nombre,Ext) -> FS;
                 C :: Nombre -> [FS] -> FS }
  deriving (Eq, Show)

----
-- 1
cjazz :: FS 
cjazz = C "jazz" [A ("mumbles", Mp3)]

crock :: FS 
crock = C "rock" [ A ("clones", Mp3), A ("bajan", Mp3), A ("clara", Mp3)]

cmusica :: FS
cmusica = C "musica" [cjazz, crock, A ("clara", Mp3)]


-- Completar el resto de los componentes del FS

cort :: FS
cort = C "ort" [cobls, A ("notas", Txt)] 

cobls :: FS
cobls = C "obls" [A ("p2", Txt), A ("p2", Jar), A ("fc", Hs)]

csys :: FS
csys = C "sys" [A ("sys", Txt), C "sys" []]

csysvacio :: FS
csysvacio = C "sys" []

craiz :: FS
craiz = C "raiz" [cmusica, A ("notas", Txt), cort, csys]

----
-- 2
nombre :: FS -> Nombre
nombre = \fs -> case fs of { A (n, e) -> n ++ "." ++ show e;
                             C nom fss ->  nom
						   }
----
-- 3

contenido :: FS -> [Nombre]
contenido = \fs -> case fs of { A (n, e) -> error "no es carpeta";
	                            C n fss -> case fss of {				-- C n [fss] seria el FS tipo carpeta a 'recorrer'
									                    [] -> [];		--caso para una carpeta vacia
														x:xs -> [nombre x] ++ contenido (C "" xs)		--caso con un elemento x (de tipo FS) y concatenacion con un nuevo FS simulado con el resto de elementos
														}
                              }

----
-- 4
cantA :: FS -> Int 
cantA = undefined
----
-- 5
pertenece :: Nombre -> FS -> Bool 
pertenece = undefined
----
-- 6
valido :: FS-> Bool
valido = undefined
----
-- 7
archivosExt :: Ext -> FS -> [Nombre]
archivosExt = undefined
----
-- 8
cambiarNom :: Nombre -> Nombre -> FS -> FS 
cambiarNom = undefined		
----
-- 9
nivelesC :: FS -> Int
nivelesC = undefined	
----
-- 10
borrar :: Nombre -> FS -> FS 
borrar = undefined
----
-- 11
ordenar :: FS-> FS
ordenar = undefined


