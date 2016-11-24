-- Orden_terminos.hs
-- Implementación de los órdenes de caminos recursivos 
-- Sevilla, 14 de Junio de 2016
-- ------------------------------------------------------------------

module OrdenTerminos where

import Orden    ( ordLex )
import Terminos ( Termino (..)
                , ocurre) 

-- | (ordenPorLista xs a b) es el resultado de comparar a y b, tal que
-- a > b syss a aparece antes en xs que b
--    >>> ordenPorLista ["a","b","c"] "a" "c"
--    GT
--    >>> ordenPorLista ["a","b","c"] "c" "b"
--    LT
--    >>> ordenPorLista ["a","b","c"] "b" "b"
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
                
-- | (ordenCamLex ord s t) es el resultado de comparar s y t con el
-- orden de caminos lexicográfico inducido por ord.
--    >>> let ord = ordenPorLista ["i","f","e"]
--    >>> ordenCamLex ord (T "f" [V ("x",1), T "e" []]) (V ("x",1))
--    GT
--    >>> ordenCamLex ord (T "i" [T "e" []]) (T "e" [])
--    GT
--    >>> ordenCamLex ord (T "i" [T "f" [V("x",1),V("y",1)]]) (T "f" [T "i" [V("y",1)], T "i" [V("x",1)]])
--    GT
--    >>> ordenCamLex ord (T "f" [V("y",1),V("z",1)])  (T "f" [T "f" [V("x",1),V("y",1)], V("z",1)])
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

-- | (ordenTerminoLex t s) es el resultado de comparar el nombre del
-- elemento de la posición vacía mediante el orden alfabético. Por
-- ejemplo,
--    >>> ordenTermino (V ("a",2)) (V ("b",1))
--    LT
--    >>> ordenTermino (V ("x",2)) (T "f" [V ("b",1)])
--    GT
--    >>> ordenTermino (T "g" [V("x",2)]) (T "f" [V("b",1)])
--    GT
ordenTermino::  Termino -> Termino -> Ordering
ordenTermino (V (a,_)) (V (b,_)) = compare a b
ordenTermino (V (a,_)) (T b _) = compare a b
ordenTermino (T a _) (V (b,_)) = compare a b
ordenTermino (T a _) (T b _) = compare a b
                
-- | (ordenCamRec stat ord t s) es el resultado de comparar s y t con el
-- orden de caminos recursivo, inducido por orden ord. Por ejemplo,
--    >>> let stat f (ordenTermino) t s = ordLex ordenTermino t s 
--    >>> let ord = ordenPorLista ["i","f","e"]
--    >>> ordenCamRec stat ord (T "f" [V ("x",1), T "e" []]) (V ("x",1))
--    GT
--    >>> ordenCamRec stat ord (T "i" [T "e" []]) (T "e" [])
--    GT
--    >>> ordenCamRec stat ord (T "i" [T "f" [V("x",1),V("y",1)]]) (T "f" [T "i" [V("y",1)], T "i" [V("x",1)]])
--    GT
--    >>> ordenCamRec stat ord (T "f" [V("y",1),V("z",1)])  (T "f" [T "f" [V("x",1),V("y",1)], V("z",1)])
--    LT
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
  
-- Comprobación doctest
-- Examples: 34  Tried: 34  Errors: 0  Failures: 0

