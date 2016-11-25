-- ParesCriticos.hs
-- Implementación de los pares críticos
-- Sevilla, 6 de Noviembre de 2016
-- ------------------------------------------------------------------

module Orden where
    
import Terminos

-- | (indiceMaximo t) es el mayor índice que aparece en t. Por ejemplo,
--    >>> indiceMaximo (V ("x",3))
--    3
--    >>> indiceMaximo (T "w" [V("x",4), V("z", 200), V("y", 1)])
--    200
indiceMaximo :: Termino -> Indice
indiceMaximo (V (_,i)) = i
indiceMaximo (T _ ts) = maximum (map indiceMaximo ts)

-- | (renombraTermino n t) es el termino resultante tras sumar a todos los
-- índices de t el entero n. Por ejemplo,
--    >>> renombraTermino 5 (V ("x",3)) 
--    V ("x",8)
--    >>> renombraTermino 3 (T "w" [V("x",4), V("z", 200), V("y", 1)])
--    T "w" [V ("x",7),V ("z",203),V ("y",4)]
renombraTermino :: Int -> Termino -> Termino
renombraTermino n (V (x,i)) = V(x,i+n)
renombraTermino n (T f ts) = T f (map (renombraTermino n) ts)

-- | (parCritico c (l1,r1) (l2,r2) calcula el par crítico de la regla l1 ->
-- r1 y l2 -> r2. Por ejemplo,
--    >>> let c a = a
--    >>> parCritico c (V ("x",1), T "f" [V ("y",1)]) (V ("x",2),T "g" [V("y",2)])
--    [(T "f" [V ("y",1)],T "g" [V ("y",2)])]
--    >>> parCritico c (T "f" [V ("x",1)],T "f" [V ("y",1)]) (V ("x",1), T "g" [V("y",2)])
--    []
parCritico :: (Termino -> Termino) -> (Termino, Termino)
           -> (Termino, Termino) -> [(Termino, Termino)]
parCritico c (l1, r1) (l2, r2)
    | sigma1 == Left UNIFICACION = []
    | otherwise = [(sigma r1, sigma (c r2))]
    where sigma1 = unificacion l1 l2
          sigma = aplicaTerm (elimR(sigma1))
          elimR (Right a) = a
                            
-- | (paresCriticos rlista (l,r)) calcula la lista de todos los pares críticos
-- formados al unificar rlista hacia la izquierda con un subtérmino l, donde
-- l->r es una regla. Por ejemplo,
--    >>> paresCriticos [(V ("x",1), T "f" [V("y",1)])] (V ("x",2), T "g" [V("y",2)])
--    []
--    >>> paresCriticos [(T "f" [V ("x",1)], T "f" [V ("y",1)]),(T "f"[V ("x",1)], T "g" [V ("x",2)]), (T "g" [V ("x",2)],  T "f" [V ("y",1)])] (V ("x",1), T "f" [V("y",1)])
--    []
--    >>> paresCriticos [(T "f" [T "g" [V("x",1),V("y",1)],V("z",1)],T "g" [V("x",1),V("z",1)]), (T "g" [V("x",1),V("y",1)],V("x",1))] (T "g" [V("x",1),V("y",1)],T "g" [V("x",1),V("z",1)])
--    [(T "g" [V ("x",1),V ("z",3)],V ("x",1))]
 
paresCriticos :: [(Termino, Termino)] -> (Termino, Termino)
                 -> [(Termino, Termino)]
paresCriticos rlista (l,r) =
    cps (\x -> x) (renombraTermino m l) (renombraTermino m r)
    where cps _ (V _) _ = []
          cps c ter@(T f ts) r1 = concat(map (parCritico c (ter,r1)) rlista) ++
                                             (pcint c f [] ts r1)
          pcint _ _ _ [] _ = []
          pcint c f ts0 (t:ts1) r2 =
              (cps (\s -> c (T f (ts0++[s]++ts1))) t r2)
                          ++ (pcint c f (ts0++[t]) ts1 r2)
          m = maximum(map
                      (\(l3,r3) -> maximum(indiceMaximo l3, indiceMaximo r3))
                       rlista) + 1

-- | (paresLista r) es la lista de todos los pares críticos de R,
-- incluyendo los triviales. Por ejemplo,
--    >>> paresLista [(V ("x",1), T "f" [V("y",1)])]
--    []
--    >>> paresLista [(T "f" [T "g" [V("x",1),V("y",1)],V("z",1)],T "g"[V("x",1),V("z",1)]), (T "g" [V("x",1),V("y",1)],V("x",1))]
--    [(T "g" [V ("x",1),V ("z",1)],T "g" [V ("x",1),V ("z",1)]),(T "g" [V ("x",1),V ("z",3)],T "f" [V ("x",1),V ("z",3)]),(V ("x",1),V ("x",1))]
paresLista :: [(Termino, Termino)] -> [(Termino, Termino)]
paresLista r1 = paresLista2 r1 r1


-- | (paresLista2 r1 r2) es la lista de todos los pares críticos al
-- unificar los términos a la izquierda de r1 con un subtérmino a la
-- izquierda con r2. Por ejemplo,
--    >>> paresLista2 [(V ("x",1), T "f" [V("y",1)])] [(V ("x",1), T "f"[V("y",1)])]
--    []
--    >>> paresLista2 [(T "f" [T "g" [V("x",1),V("y",1)],V("z",1)],T "g"[V("x",1),V("z",1)]), (T "g" [V("x",1),V("y",1)],V("x",1))]  [(V ("x",1), T "f" [V("y",1)])] 
--    []
--    >>> paresLista2 [(V ("x",1), T "f" [V("y",1)])] [(T "f" [T "g" [V("x",1),V("y",1)],V("z",1)],T "g"[V("x",1),V("z",1)]), (T "g" [V("x",1),V("y",1)],V("x",1))] 
--    [(T "g" [V ("x",3),V ("z",3)],T "f" [V ("y",1)]),(T "g" [V ("x",3),V ("z",3)],T "f" [T "f" [V ("y",1)],V ("z",3)]),(V ("x",3),T "f" [V ("y",1)])]
paresLista2 :: [(Termino, Termino)] -> [(Termino, Termino)]
               -> [(Termino, Termino)]
paresLista2 r1 r2 = concat(map(paresCriticos r1) r2)

-- Comprobación doctest
-- Examples: 70  Tried: 70  Errors: 0  Failures: 0

