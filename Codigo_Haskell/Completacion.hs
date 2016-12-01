-- Terminos.hs
-- Términos de primer orden.
-- Sevilla, 26 de Noviembre de 2016
-- ---------------------------------------------------------------------

module Completacion where

import Orden
import Terminos
import OrdenTerminos
import ParesCriticos

-- |(minRegla rl n r1 r2) divide un conjunto de reglas en dos, donde uno
-- tiene el tamaño de r1. Por ejemplo,
--    >>> minRegla (V("x",1),V("x",2)) 2 [(V("y",1),V("y",2))] []
--    ((V ("x",1),V ("x",2)),[(V ("y",1),V ("y",2))])
--    >>> minRegla (V("x",1),V("x",2)) 1 [(V("y",1),V("y",2))] []
--    ((V ("x",1),V ("x",2)),[(V ("y",1),V ("y",2))])
--    >>> minRegla (V ("x",1),V ("x",2)) 2 [(V ("y",1),V ("y",2)),(T "f" [V("z",1),V("z",2)], V ("x",2))] []
--    ((V ("x",1),V ("x",2)),[(T "f" [V ("z",1),V ("z",2)],V ("x",2)),(V ("y",1),V ("y",2))])
--    >>> minRegla (V ("x",1),V ("x",2)) 4 [(V ("y",1),V ("y",2)),(T "f" [V("z",1),V("z",2)], V ("x",2))] []
--    ((V ("y",1),V ("y",2)),[(T "f" [V ("z",1),V ("z",2)],V ("x",2)),(V ("x",1),V ("x",2))])

minRegla :: (Termino, Termino) -> Int -> [(Termino, Termino)]
     -> [(Termino, Termino)]
     -> ((Termino, Termino), [(Termino, Termino)])
minRegla rl _ [] r2 = (rl,r2)
minRegla rl n ((l,r):r1) r2
    | m < n = minRegla (l,r) m r1 (rl:r2)
    | otherwise = minRegla rl n r1 ((l,r):r2)
    where m = longitudTerm l + longitudTerm r

-- |(escogeRegla r) calcula la regla que hay que aplicar durante el
-- algoritmo de Huet. Por ejemplo,
--    >>> escogeRegla [(V("y",1),V("y",2))]
--    ((V ("y",1),V ("y",2)),[])
--    >>> escogeRegla [(V ("y",1),V ("y",2)),(T "f" [V("z",1),V("z",2)], V ("x",2))]
--    ((V ("y",1),V ("y",2)),[(T "f" [V ("z",1),V ("z",2)],V ("x",2))])

escogeRegla :: [(Termino, Termino)]
            -> ((Termino, Termino), [(Termino, Termino)])
escogeRegla [] = error("No se puede aplicar ninguna regla")
escogeRegla ((l,r):r1) = minRegla (l,r) (longitudTerm l + longitudTerm r) r1 []

-- |(anadeRegla (l,r) e s r) es el paso (e) del algoritmo de Huet.
anadeRegla :: (Termino, Termino) -> [(Termino, Termino)] -> [Ecuacion]
     -> [Ecuacion] -> ([(Termino, Termino)], [(Termino, Termino)],
         [(Termino, Termino)])
anadeRegla (l,r) e s r1 = (e2, (l,r):s1, r2)
    where (e1,s1) = simpl l r s r1 s e []
          (e2,r2) = simpl l r s r1 r1 e1 []
simpl
  :: Termino
     -> Termino
     -> [Ecuacion]
     -> [Ecuacion]
     -> [(Termino, Termino)]
     -> [(Termino, Termino)]
     -> [(Termino, Termino)]
     -> ([(Termino, Termino)], [(Termino, Termino)])
simpl _ _ _ _ [] a b = (a,b)
simpl l r s r1 ((g,d):u) e1 u1 
    | g1 == g =
        simpl l r s r1 u e1 ((g, d1):u1)
    | otherwise = simpl l r r1 s u ((g1,d):e1) u1
       where g1 = formaNormal [(l,r)] g
             d1 = formaNormal ((l,r):(r1++s)) d

-- |(orienta ord trip) es el segundo bucle del algoritmo de Huet.
orienta :: (Termino -> Termino -> Ordering)
     -> ([(Termino, Termino)],  [(Termino, Termino)], [(Termino, Termino)])
     -> ([(Termino, Termino)], [(Termino, Termino)])
orienta _ ([],ss,rr) = (ss,rr)
orienta ord ((s,t):ee,ss,rr)
    | s1 == t1 = orienta ord (ee,ss,rr)
    | ord s1 t1 == GT = orienta ord (anadeRegla (s1,t1) ee ss rr)
    | ord t1 s1 == GT = orienta ord (anadeRegla (t1,s1) ee ss rr)
    | otherwise = error("Error con la funcion ord")
    where s1 = formaNormal (rr++ss) s
          t1 = formaNormal (rr++ss) t

-- |(completa ord ee) es el resultado de aplicar a ee, el algoritmo de
-- completación de Huet. Por ejemplo,
--    >>> let ord = ordenCamLex (ordenPorLista ["a","b","c","x","y","z"])
--    >>> completa ord [(V("x",1),V("x",1))]
--    []
--    >>> completa ord [(V ("y",1),V ("y",2)),(T "f" [V("y",1),V("y",2)], V ("x",2))]
--    *** Exception: Error con la funcion ord
--    >>> completa ord [(T "f" [V("x",1),V("x",2)], V ("x",2))]
--    [(T "f" [V ("x",1),V ("x",2)],V ("x",2))]
completa :: (Termino -> Termino -> Ordering)
            -> [(Termino, Termino)] -> [(Termino, Termino)]
completa ord ee = compl ord (ee,[],[])
compl :: (Termino -> Termino -> Ordering)
     -> ([(Termino, Termino)], [(Termino, Termino)],[(Termino, Termino)])
     -> [(Termino, Termino)]
compl ord (ee,ss,tt) = case orienta ord (ee,ss,tt) of
                         ([],rr1)  -> rr1
                         (ss1,rr1) -> compl ord (paCr,ss2,rl:rr1)
                                      where (rl, ss2) = escogeRegla ss1
                                            paCr = (paresLista2 [rl] rr1)++
                                                   (paresLista2 rr1 [rl])++
                                                   (paresLista2 [rl] [rl])
          
-- Comprobación doctest
-- Examples: 115  Tried: 115  Errors: 0  Failures: 0

