distancia :: N -> N -> N
distancia = \m n -> case m of {
    O -> n;
    S x -> case n of {
        O -> m;
        S y -> distancia x y;
    }
}
{---------------------------------------------
(ꓯ u, w :: N) distancia u w = distancia w u
Dem por induccion en u :: N
Caso u=0) (ꓯ w :: N) distancia O w = distancia w O
    Dem por induccion en w :: N
    Caso w=0)  distancia O O = distancia O O
                      Se cumple por reflexividad del =
    Caso w= S x, con x :: N cualquiera
        HI1) distancia O x = distancia x O
        TI1) distancia O (S x) = distancia (S x) O
                    (def distancia, Bx2, case x2) x 2
                S x       =     S x
                         = x RME
Caso u= S y, con y::N cualquiera
    HI2)(ꓯ w :: N) distancia y w = distancia w y
    TI2)(ꓯ w :: N) distancia (S y) w = distancia w (S y)
    Dem por induccion en w :: N
        Caso w=0) distancia (S y) O = distancia O (S y)
                (def distancia, Bx2, case / case x2) x 2
                S y         =       S y
                           RME
        
        Caso w= S x, con x :: N cualquiera
        HI3) distancia (S y) x = distancia x (S y)
        TI3) distancia (S y) (S x) = distancia (S x) (S y)
            (def distancia, Bx2, case x2) x 2
            distancia y x = distancia x y
            Se cumple por HI2 con w = x

---------------------------------------------------
(ꓯ u, w :: N) distancia u (u + w) = w
Dem por induccion en u :: N. Sea w::N cualquiera
Caso u=0) (ꓯ w :: N) distancia O (O + w) = w
	Sea w :: N
                def +, Bx2 case
                distancia O w = w
                def distancia, Bx2, case
                w = w
                 RME

Caso u= S x, con x::N cualquiera
    HI) (ꓯ w :: N) distancia x (x + w) = w
    TI) (ꓯ w :: N) distancia (S x) ((S x) + w) = w
	Sea w :: N		
                    distancia (S x) ((S x) + w) = w
					def +, Bx2 case x2
                    distancia (S x) (S(x + w)) = w
                    def distancia, Bx2, case x2
                    distancia x (x + w) = w
                    HI

-----------------------------------------------------
(ꓯ u, w :: N) min u w + distancia u w = max u w
Dem por induccion en u :: N
Caso u=0) (ꓯ w :: N) min O w + distancia O w = max O w.
                def min, Bx2, case
                O + distancia O w = max O w
                def distancia, Bx2, case
                O + w = max O w
                def max, Bx2, case
                O + w = w
                def +, Bx2, case
                w = w
                 RME

Caso u= S x, con x::N cualquiera
    HI1) (ꓯ w :: N) min x w + distancia x w = max x w
    TI1) (ꓯ w :: N) min (S x) w + distancia (S x) w = max (S x) w
        Dem por induccion en w :: N
        Caso w=0) 
			min (S x) O + distancia (S x) O = max (S x) O
            def min, Bx2, case
            O + distancia (S x) O = max (S x) O
            def distancia, Bx2, case
            O + S x = max (S x) O
            def max, Bx2, case
            O + S x = S x
            def +, Bx2, case
            S x = S x
                RME
        Caso w= S y, con y :: N cualquiera
            HI2) min (S x) y + distancia (S x) y = max (S x) y
            TI2) min (S x) (S y) + distancia (S x) (S y) = max (S x) (S y)
                def min, Bx2, case x2
                S (min x y) + distancia (S x) (S y) = max (S x) (S y)
                def distancia, Bx2, case x2
                S (min x y) + distancia x y = max (S x) (S y)
                def max, Bx2, case x2
                S (min x y) + distancia x y = S (max x y)
                def +, Bx2, case
                S (min x y + distancia x y) = S (max x y)
                HI1 con w=y
                S (max x y) = S (max x y)
                RME
				
---------------------------------------------------------------}
