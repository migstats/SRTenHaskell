-- ParesCriticos.hs
-- Implementación de los pares críticos
-- Sevilla, 6 de Noviembre de 2016
-- ------------------------------------------------------------------

import Terminos

-- (indiceMaximo t) es el mayor índice que aparece en t. Por ejemplo,
--    λ> indiceMaximo (V ("x",3))
--    3
--    λ> indiceMaximo (T "w" [V("x",4), V("z", 200), V("y", 1)])
--    200
indiceMaximo :: Termino -> Indice
indiceMaximo (V (_,i)) = i
indiceMaximo (T _ ts) = maximum (map indiceMaximo ts)

-- (renombraTermino n t) es el termino resultante tras sumar a todos los
-- índices de t el entero n. Por ejemplo,
--    λ> renombraTermino 5 (V ("x",3)) 
--    V ("x",8)
--    λ> renombraTermino 3 (T "w" [V("x",4), V("z", 200), V("y", 1)])
--    T "w" [V ("x",7),V ("z",203),V ("y",4)]
renombraTermino :: Int -> Termino -> Termino
renombraTermino n (V (x,i)) = V(x,i+n)
renombraTermino n (T f ts) = T f (map (renombraTermino n) ts)

-- (parCritico c l1 r1 l2 r2) calcula el par crítico de la regla l1 ->
-- r1 y l2 -> r2. Por ejemplo,
--    λ> let c a = a
--    λ> parCritico c (V ("x",1)) (T "f" [V ("y",1)]) (V ("x",2)) (T "g" [V("y",2)])
--    [(T "f" [V ("y",1)],T "g" [V ("y",2)])]
--    λ> parCritico c (T "f" [V ("x",1)]) (T "f" [V ("y",1)]) (V ("x",1)) (T "g" [V("y",2)])
--    []
parCritico :: (Termino -> Termino)
     -> Termino -> Termino -> Termino -> Termino -> [(Termino, Termino)]
parCritico c l1 r1 l2 r2
    | sigma1 == Left UNIFICACION = []
    | otherwise = [(sigma r1, sigma (c r2))]
    where sigma1 = unificaEjem l1 l2
          sigma = aplicaTerm (elimR(sigma1))
          elimR (Right a) = a
                            
-- (paresCriticos R l r) calcula la lista de todos los pares críticos
-- formados al unificar R hacia la izquierda con un subtérmino l, donde
-- l->r es una regla.

