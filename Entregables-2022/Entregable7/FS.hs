{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module FS where 

import Prelude

-- Nombre: Estudiante 1
-- Número: Estudiante 1

-- Nombre: Estudiante 2
-- Número: Estudiante 2


type Nombre = String 

data Ext where { Txt::Ext ; Mp3::Ext ; Jar::Ext ; Doc::Ext ; Hs::Ext }
  deriving (Eq, Show)


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
cort = undefined

cobls :: FS
cobls = undefined

csys :: FS
csys = undefined

craiz :: FS
craiz = undefined

----
-- 2
nombre :: FS -> Nombre
nombre = undefined
----
-- 3
contenido :: FS-> [Nombre]
contenido = undefined
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


