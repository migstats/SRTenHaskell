-- terminos_robison.hs
-- Implementación del algorítmo de Robinson
-- Sevilla, 14 de Junio de 2016
-- ---------------------------------------------------------------------

import Data.Maybe
import Terminos

-- (resuelve2 ts) es la sustitución resultante para el problema de
-- unificación ts.
--    λ> resuelve2 [(V ("x",2), V ("x",2))]
--    Just []
--    λ> resuelve2 [(V ("x",2), V ("x",3))]
--    Just [(V ("x",3),V ("x",2))]
--    λ> resuelve2 [(V ("x",2), V ("x",3))] 
--    Just [(V ("x",3),V ("x",2))]
--    λ> resuelve2 [(V ("x",2), T "f" [V ("x",2)])]
--    Nothing
--    λ> resuelve2 [(V ("x",2),T "f" [V ("x",3)]),(V ("x",3),T "f" [V ("x",4)])] 
--    Just [(T "f" [T "f" [V ("x",4)]],V ("x",2)),(T "f" [V ("x",4)],V ("x",3))]
--    λ> resuelve2 [(V ("x",1), T "f" [V ("a",1)]),(T "g" [V ("x",1),V ("x",1)], T "g" [V ("x",1),V ("y",1)])]
--    Just [(T "f" [V ("a",1)],V ("x",1)),(T "f" [V ("a",1)],V ("y",1))]

resuelve2:: [(Termino,Termino)] -> Maybe [(Termino, Termino)]
resuelve2 [(t1, t2)] = unificaTerm t1 t2
resuelve2 ((t1,t2):ts)
  | Nothing == subs1 = Nothing
  | otherwise        = subs3
  where
    subs1 = unificaTerm t1 t2
    subs1' = fromJust subs1
    subs_tms1 = aplicaSust2 subs1' (fst $ unzip ts)
    subs_tms2 = aplicaSust2 subs1' (snd $ unzip ts)
    subs2 = resuelve2 (zip subs_tms1 subs_tms2)
    subs3 = comboSust subs1 subs2

-- -- Con las funciones auxiliares--------------------------------------------

-- (unificaTerm t1 t2) es la sustitución basada en t1 y t2.
--    λ> unificaTerm (V ("x",2)) (T "a" [])
--    Just [(T "a" [],V ("x",2))]
--    λ> unificaTerm (V ("x",2)) (T "a" [V ("y",3)])
--    Just [(T "a" [V ("y",3)],V ("x",2))]
--    λ> unificaTerm (T "a" [V ("y",3)]) (T "a" [])
--    Nothing
unificaTerm :: Termino -> Termino -> Maybe [(Termino,Termino)]
unificaTerm (T a []) (T b [])
  | a == b    = Just []
  | otherwise = Nothing
unificaTerm v1@(V _) v2@(V _)
  | v1 == v2  = Just []
  | otherwise = Just [(v2,v1)]
unificaTerm c@(T _ []) v@(V _) = Just [(c,v)]
unificaTerm v@(V _) c@(T _ []) = Just [(c,v)]
unificaTerm (T _ []) (T _ _) = Nothing
unificaTerm (T _ _) (T _ []) = Nothing
unificaTerm f@(T _ _) v@(V a)
  | contenidoVar a f = Nothing
  | otherwise        = Just [(f,v)]
unificaTerm v@(V a) f@(T _ _)
  | contenidoVar a f = Nothing
  | otherwise        = Just [(f,v)]
unificaTerm (T n1 l1) (T n2 l2)
  | n1 /= n2                   = Nothing
  | (length l1) /= (length l2) = Nothing
  | otherwise                  = resuelve2 $ zip l1 l2

aplicaSust2 :: [(Termino,Termino)] -> [Termino] -> [Termino]
aplicaSust2 [] t           = t
aplicaSust2 (s:[]) t       = map (aplicaUnaSust s) t
aplicaSust2 (s:ss@(_:_)) t = aplicaSust2 ss (map (aplicaUnaSust s) t)

aplicaUnaSust :: (Termino,Termino) -> Termino -> Termino
aplicaUnaSust (t, v1@(V a)) v2@(V _)
    | contenidoVar a v2        = t
    | otherwise                = v2
aplicaUnaSust (_, V _) c@(T _ []) = c
aplicaUnaSust s@(t, V _) (T f ts) =
    T f (map (aplicaUnaSust s) ts)

comboSust :: Maybe [(Termino,Termino)] -> Maybe [(Termino,Termino)]
  -> Maybe [(Termino,Termino)]
comboSust _ Nothing                     = Nothing
comboSust sust1 (Just [])               = sust1
comboSust (Just []) sust2@(Just (_:_))  = sust2
comboSust (Just sust1@(_:_)) (Just sust2@(_:_))   = Just (s1 ++ sust2)
    where tsc1  = map fst sust1
          vsc1  = map snd sust1
          tsc1' = aplicaSust2 sust2 tsc1
          s1    = zip tsc1' vsc1

-- ---------------------------------------------------------------------
