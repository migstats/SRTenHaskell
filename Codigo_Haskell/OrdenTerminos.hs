-- Orden_terminos.hs
-- Implementación de los órdenes de caminos recursivos 
-- Sevilla, 14 de Junio de 2016
-- ------------------------------------------------------------------

module OrdenTerminos where

import Orden    ( ordLex )
import Terminos ( Termino (..)
                , ocurre) 

-- (ordenPorLista xs a b) es el resultado de comparar a y b, tal que
-- a > b syss a aparece antes en xs que b
--    λ> ordenPorLista ["a","b","c"] "a" "c"
--    GT
--    λ> ordenPorLista ["a","b","c"] "c" "b"
--    LT
--    λ> ordenPorLista ["a","b","c"] "b" "b"
--    EQ
ordenPorLista :: Ord a => [a] -> a  -> a -> Ordering
ordenPorLista [] _ _ =
    error("Ninguno de los dos elementos se encuentran en la lista")
ordenPorLista (x:xs) a b
  | a == x = if a == b
             then EQ
             else GT
  | b == x = LT
  | otherwise = ordenPorLista xs a b


-- (ordenCamLex ord s t) es el resultado de comparar s y t con el
-- orden de caminos lexicográfico inducido por ord.
--    λ> let ord = ordenPorLista ["i","f","e"]
--    λ> ordenCamLex ord (T "f" [V ("x",1), T "e" []]) (V ("x",1))
--    GT
--    λ> ordenCamLex ord (T "i" [T "e" []]) (T "e" [])
--    GT
--    λ> ordenCamLex ord (T "i" [T "f" [V("x",1),V("y",1)]]) (T "f" [T "i" [V("y",1)], T "i" [V("x",1)]])
--    GT
--    λ> ordenCamLex ord (T "f" [V("y",1),V("z",1)])  (T "f" [T "f" [V("x",1),V("y",1)], V("z",1)])
--    LT
ordenCamLex:: ([Char] -> [Char] -> Ordering) -> Termino -> Termino -> Ordering
ordenCamLex _ s (V x)
  | s == (V x) = EQ
  | ocurre x s = GT --OCL1
  | otherwise = LT
ordenCamLex _ (V _) (T _ _) = LT
ordenCamLex ord s@(T f ss) t@(T g ts) --OCL2
  | all (\x -> ordenCamLex ord x t == LT) ss
    = case ord f g of
      GT -> if all (\x -> ordenCamLex ord s x == GT) ts
            then GT --OCL2b
            else LT
      EQ -> if all (\x -> ordenCamLex ord s x == GT) ts
            then ordLex (ordenCamLex ord) ss ts --OCL2c
            else LT
      LT -> LT  
  | otherwise = GT --OCL1a

-- (ordenCamRec ord t s) es el resultado de comparar s y t con el
-- orden de caminos recursivo, inducido por orden ord.
ordenCamRec:: ([Char] -> (Termino -> Termino -> Ordering) -> [Termino] -> [Termino] -> Ordering)
           ->  ([Char] -> [Char] -> Ordering)
           -> Termino -> Termino -> Ordering
ordenCamRec _ _ s (V x)
  | s == (V x) = EQ
  | ocurre x s = GT --OCR1
  | otherwise = LT
ordenCamRec _ _ (V _) (T _ _) = LT
ordenCamRec est ord s@(T f ss) t@(T g ts) --OCR2
  | all (\x -> ordenCamRec est ord x t == LT) ss
    = case ord f g of
      GT -> if all (\x -> ordenCamRec est ord s x == GT) ts
            then GT --OCR2b
            else LT
      EQ -> if all (\x -> ordenCamRec est ord s x == GT) ts
            then est f (ordenCamRec est ord) ss ts --OCR2c
            else LT
      LT -> LT
  | otherwise = GT --OCR2a
  

