-- Orden.hs
-- Implementación de relaciones de orden del capítulo 1
-- Sevilla, 4 de Mayo de 2016
-- ---------------------------------------------------------------------

module Orden where

-- Referencias
-- ===========

-- + "On Multiset Orderings" http://bit.ly/1VXmgZO
-- + "Data.MultiSet Functions" http://bit.ly/1YitFkk
import Data.Ord()
import qualified Data.MultiSet as M

-- Funciones útiles
-- ================

-- vacio es el multiconjunto vacío. Por ejemplo,
--    λ> vacio
--    fromOccurList []
vacio :: M.MultiSet a
vacio = M.empty

-- (unitario x) es el multiconjunto cuyo único elemento es x. Por
-- ejemplo, 
--    λ> unitario 5
--    fromOccurList [(5,1)]
unitario :: a -> M.MultiSet a
unitario = M.singleton

-- (inserta x m) es el multiconjunto obtenido añadiendo el elemento x al
-- multiconjunto m. Por ejemplo,
--    λ> inserta 4 (unitario 5)
--    fromOccurList [(4,1),(5,1)]
--    λ> inserta 4 it
--    fromOccurList [(4,2),(5,1)]
inserta :: Ord a => a -> M.MultiSet a -> M.MultiSet a
inserta = M.insert

-- (lista2Multiconj xs) es el multiconjunto cuyos elementos son los de la
-- lista xs. Por ejemplo,
--    λ> lista2Multiconj [4,5,4]
--    fromOccurList [(4,2),(5,1)]
lista2Multiconj:: Ord a => [a] -> M.MultiSet a
lista2Multiconj = M.fromList

-- (ordLex r xs ys) es el orden lexicográfico inducido por r sobre xs
-- e ys. Por ejemplo,
--    λ> ordLex compare [1,2] [1,5]
--    LT
--    λ> ordLex compare [1,2] [1,2]
--    EQ
--    λ> ordLex compare [3,1] [2,4]
--    GT
--    λ> ordLex compare [1] [1,3]
--    LT
--    λ> ordLex compare [1,3] [1]
--    GT
--    λ> let r x y = compare (abs x) (abs y)
--    λ> ordLex compare [-4,3] [2,5]
--    LT
--    λ> ordLex r       [-4,3] [2,5]
--    GT
ordLex :: (a -> b -> Ordering) -> [a] -> [b] -> Ordering
ordLex _ [] []  = EQ
ordLex _ [] _  = LT
ordLex _ _  []  = GT
ordLex r (x:xs) (y:ys) = 
    case a of 
      EQ -> ordLex r xs ys
      _  -> a 
    where a = r x y

-- (borraelem a mult) es la lista obtenida eliminando el primer
-- elemento de xs igual a x. Por ejemplo,
--    λ> borraElem 2 (lista2Multiconj [3,2,5,2])
--    fromOccurList [(2,1),(3,1),(5,1)]
--    λ> borraElem 4 (lista2Multiconj [3,2,5,2])
--    fromOccurList [(2,2),(3,1),(5,1)]
borraElem :: Ord a => a -> M.MultiSet a -> M.MultiSet a
borraElem = M.delete

-- (difMulticonj xs ys) es la diferencia de los multiconjuntos xs e ys
-- Por ejemplo,
--    λ> difMulticonj (lista2Multiconj [1,2,3,2,5]) (lista2Multiconj [2,7,5,7])
--    fromOccurList [(1,1),(2,1),(3,1)]
--    λ> difMulticonj (lista2Multiconj [2,7,5,7]) (lista2Multiconj [1,2,3,2,5])
--    fromOccurList [(7,2)]
difMulticonj :: Ord a => M.MultiSet a -> M.MultiSet a -> M.MultiSet a
difMulticonj  = M.difference

-- Dado un orden > en un conjunto A, se define el orden >' sobre el
-- conjunto M(A) de los multiconjuntos de A como sigue:
--    M >' N syss existen X, Y ∈ M(A) tales que se cumplen las
--    siguientes condiciones
--    1. ∅ ≠ X ⊆ M 
--    2. N = (M - X) ∪ Y 
--    3. (∀y∈Y)(∃x∈X)[x > y]            
-- Por ejemplo, {5,3,1,1} >' {4,3,3,1} ya que si X = {5,1} e Y = {4,3}
-- entonces 
--    1. ∅ ≠ {5,1} ⊆ {5,3,1,1} 
--    2. {4,3,3,1} = ({5,3,1,1} - {5,1}) ∪ {4,3} 
--    3. (∀y∈{4,3})(∃x∈{5,1})[x > y]            

-- (ordenMulticonj ms ns) es la comparación de los multiconjuntos ms y ns.
-- Por ejemplo,
--    ghci> let ms = lista2Multiconj [5,3,1,1] 
--    ghci> let ns = lista2Multiconj [4,3,3,1]
--    ghci> ordenMulticonj ms ns
--    GT
ordenMulticonj :: Ord a => M.MultiSet a -> M.MultiSet a -> Ordering
ordenMulticonj ms ns
  | ms == ns                              = EQ
  | all (\y -> (any (\x -> x > y) xs)) ys = GT
  | otherwise                             = LT  
  where xs = ms `difMulticonj` ns
        ys = ns `difMulticonj` ms
