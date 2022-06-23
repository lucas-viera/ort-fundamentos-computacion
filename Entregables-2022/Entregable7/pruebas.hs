module Pruebas where
import FS

--Instrucciones
--1) Poner el archivo FS.hs y este mismo (las pruebas) en el mismo directorio
--2) Abrir este archivo con ghci
--3) Para cada función fi pedida hay una serie de pruebas pi1,.. pin que pueden correrse por separado y devuelven True cuando el resultado es correcto.
--4) También hay una prueba pi que verifica todos los casos de cada función, y una prueba pTodo que verifica todas las pruebas de todas las funciones.


p21 = nombre cjazz == "jazz"
p22 = nombre craiz == "raiz"
p23 = nombre (A ("notas", Txt)) == "notas.txt"
p2 = and [p21,p22,p23]


p31 = contenido craiz == ["musica","notas.txt","ort","sys"]
p32 = contenido cjazz == ["mumbles.mp3"]
p33 = contenido cort == ["obls","notas.txt"]
p34 = contenido csys == ["sys.txt","sys"]
p35 = contenido cmusica == ["jazz","rock","clara.mp3"]
p36 = contenido cobls == ["p2.txt","p2.jar","fc.hs"]
p37 = contenido crock == ["clones.mp3","bajan.mp3","clara.mp3"]
p3 = and [p31,p32,p33,p34,p35,p36,p37]


p41 = cantA (C "sys" []) == 0
p42 = cantA csys == 1
p43 = cantA cjazz ==1
p44 = cantA crock == 3
p45 = cantA cobls == 3
p46 = cantA cort == 4
p47 = cantA cmusica == 5
p48 = cantA craiz == 11
p4 = and [p41,p42,p43,p44,p45,p46,p47,p48]


p51 = pertenece "mumbles" cort == False
p52 = pertenece "mumbles" cjazz
p53 = pertenece "sys" csys
p54 = pertenece "clones" csys == False
p55 = pertenece "clones" cmusica
p56 = pertenece "clones" crock
p57 = pertenece "p2" cobls
p58 = pertenece "fc" craiz
p5 = and [p51,p52,p53,p54,p55,p56,p57,p58]


p61 = valido craiz
p62 = valido cobls
p63 = valido (C "X" [])
p64 = valido (C "X" [ A ("a",Mp3), A ("a",Mp3) ]) == False
p65 = valido (C "X" [ C "D" [], C "D" []]) == False
p66 = valido (C "X" [ A ("a",Hs), A ("a",Mp3) ])
p67 = valido (C "X" [ C "R" [A ("a",Mp3), A ("a",Mp3)], C "S" []]) == False
p68 = valido (C "X" [ C "R" [C "D" [], C "D" []], C "S" []]) == False
p6 = and [p61,p62,p63,p64,p65,p66,p67,p68]


p71 = archivosExt Txt craiz == ["notas", "p2", "notas", "sys"]
p72 = archivosExt Txt cjazz == []
p73 = archivosExt Mp3 (C "sys" []) == []
p74 = archivosExt Jar cort == ["p2"]
p75 = archivosExt Hs craiz == ["fc"]
p76 = archivosExt Txt csys == ["sys"]
p77 = archivosExt Mp3 cjazz == ["mumbles"]
p78 = archivosExt Hs (A ("pruebas", Hs)) == ["pruebas"]
p7 = and [p71,p72,p73,p74,p75,p76,p77,p78]


p81 = cambiarNom "jazz" "blues" cjazz == (C "blues" [ A ("mumbles", Mp3)])
p82 = cambiarNom "mumbles" "song" cjazz == (C "jazz" [ A ("song", Mp3)])
p83 = cambiarNom "a" "b" (C "a" [(C "a" [])] )  == (C "b" [(C "b" [])] )
p84 = cambiarNom "sys" "s" csys == (C "s" [ A ("s", Txt), C "s" []])
p85 = cambiarNom "rock" "pop" (C "a" [ C "rock" [], C "pop" [] ])  == (C "a" [ C "pop" [], C "pop" [] ])
p86 = cambiarNom "clones" "clara" crock == (C "rock" [ A ("clara", Mp3), A ("bajan", Mp3), A ("clara", Mp3)])
p87 = cambiarNom "clones" "clara" (C "rock" [ A ("clones", Mp3), A ("bajan", Mp3), A ("clara", Txt)]) == (C "rock" [ A ("clara", Mp3), A ("bajan", Mp3), A ("clara", Txt)])
p88 = cambiarNom "viena" "faith" crock == crock
p8 = and [p81,p82,p83,p84,p85,p86,p87,p88]


p91 = nivelesC craiz == 3 
p92 = nivelesC (C "sys" []) == 1
p93 = nivelesC (A("fc", Hs)) == 0
p94 = nivelesC (C "Nivel1" [A("fc", Hs), C "Nivel2" []]) == 2
p95 = nivelesC (C "Nivel1" [A("fc", Hs),A("fc", Jar)]) == 1
p96 = nivelesC (C "Nivel1" [C "Nivel2a" [], C "Nivel2b" [C "Nivel3" []]]) == 3
p9 = and [p91,p92,p93,p94,p95,p96]


p101 = borrar "obls" cort == (C "ort" [A("notas", Txt)])
p102 = borrar "jazz" cjazz == (C "jazz" [A("mumbles", Mp3)])
p103 = borrar "sys" csys == (C "sys" [A("sys", Txt)])
p104 = borrar "sys.txt" csys == (C "sys" [C "sys" []])
p105 = borrar "p2.txt" cort == (C "ort" [C "obls" [A("p2", Jar),A("fc", Hs)], A("notas", Txt)])
p106 = not (pertenece "notas" (borrar "notas.txt" craiz) ) 
p107 = borrar "ARCHIVO_QUE_NO_EXISTE.TXT" craiz == craiz
p108 = borrar "CARPETA_QUE_NO_EXISTE" craiz == craiz
p109 = borrar "prueba.jar" (C "pruebas" [A("prueba",Jar),A("prueba",Hs)]) == (C "pruebas" [A("prueba",Hs)])
p1010 = borrar "prueba.txt" (C "pruebas" [A("prueba.txt",Jar)]) == (C "pruebas" [A("prueba.txt",Jar)])
p1011 = borrar "prueba.txt" (A("prueba",Txt)) == (A("prueba",Txt))
p10 = and [p101,p102,p103,p104,p105,p106,p107,p108,p109,p1010,p1011]


p111 = ordenar (A("file", Txt)) == A ("file",Txt)
p112 = ordenar (C "X" []) == (C "X" [])
p113 = ordenar cjazz == cjazz
p114 = ordenar csys == C "sys" [C "sys" [],A ("sys",Txt)]
p115 = ordenar cobls == C "obls" [A ("fc",Hs),A ("p2",Jar),A ("p2",Txt)]
p116 = ordenar cort == C "ort" [A ("notas",Txt),C "obls" [A ("fc",Hs),A ("p2",Jar),A ("p2",Txt)]]
p117 = ordenar crock == C "rock" [A ("bajan",Mp3),A ("clara",Mp3),A ("clones",Mp3)]
p118 = ordenar cmusica == C "musica" [A ("clara",Mp3),C "jazz" [A ("mumbles",Mp3)],C "rock" [A ("bajan",Mp3),A ("clara",Mp3),A ("clones",Mp3)]]
p119 = ordenar craiz == C "raiz" [C "musica" [A ("clara",Mp3),C "jazz" [A ("mumbles",Mp3)],C "rock" [A ("bajan",Mp3),A ("clara",Mp3),A ("clones",Mp3)]],A ("notas",Txt),C "ort" [A ("notas",Txt),C "obls" [A ("fc",Hs),A ("p2",Jar),A ("p2",Txt)]],C "sys" [C "sys" [],A ("sys",Txt)]]
p11 = and [p111,p112,p113,p114,p115,p116,p117,p118,p119]


pTodo = and [p2,p3,p4,p5,p6,p7,p8,p9,p10,p11]