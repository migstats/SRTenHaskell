-- Terminos.hs
-- Implementación de términos de primer orden del capítulo 3
-- Sevilla, 18 de Mayo de 2016
-- ---------------------------------------------------------------------

module Terminos where

import Data.Maybe()

-- Definiendo el tipo de dato Termino
-- ==================================

-- Los nombres son cadenas.
type Nombre = String

-- Los índices son números naturales.
type Indice = Int

-- Los nombres de variables son pares formado por un nombre y un índice.
type Nvariable = (Nombre,Indice)

-- Un término es una variable o un término compuesto. Por ejemplo,
--    x2          se representa por (V ("x", 2))
--    a           se representa por (T "a" [])
--    f(x2,g(x2)) se representa por (T "f" [V "x" 2, T "g" [V "x" 2]])
-- Por ejemplo,
--    λ> :t (V ("x",2))
--    (V ("x",2)) :: Termino
--    λ> :t (T "a" [])
--    (T "a" []) :: Termino
--    λ> :t (T "f" [V ("x",2), T "g" [V ("x",2)]])
--    (T "f" [V ("x",2), T "g" [V ("x",2)]]) :: Termino
data Termino = V Nvariable
             | T String [Termino]
             deriving (Eq, Show)

-- Una sustitución es una lista de pares formados por variables y
-- términos. Por ejemplo,
--    [(("x",2)::Nvariable, T "f" [V ("x",2), T "g" [V ("x",2)]])]
type Sustitucion = [(Nvariable,Termino)]

-- Los errores son causados por UNIFICACION (si el sistema no tiene
-- solución) o REGLA (si durante la reescritura no se puede aplicar
-- ninguna regla más)
data ERROR = UNIFICACION
           | REGLA
           deriving (Eq, Show)

-- (contenido v s) se verifica si la variable v está en el dominio de la
-- sustitución s. Por ejemplo,
--    λ> contenido ("x", 2) [(("x",2), T "f" [V ("x",2), T "g" [V ("x", 2)]])]
--    True
--    λ> contenido ("y", 2) [(("x",2), T "f" [V ("x",2), T "g" [V ("x", 2)]])]
--    False
contenido :: Nvariable -> Sustitucion -> Bool
contenido v xs = any (\(x,_) -> v == x) xs

-- (aplicaSust s v) es el término obtenido aplicando la sustitución s a la
-- variable v. Por ejemplo,
--    λ> aplicaSust [(("x",2), T "f" [V ("x",2), T "g" [V ("x",2)]])] ("x",2)
--    T "f" [V ("x",2),T "g" [V ("x",2)]]
--    λ> aplicaSust [(("x",2), T "f" [] )] ("x",3)
--    V ("x",3)
aplicaSust :: Sustitucion -> Nvariable -> Termino 
aplicaSust [] a = V a 
aplicaSust ((x,y):s) v 
       | x == v    = y
       | otherwise = aplicaSust s v

-- (aplicaTerm s t) es el término obtenido aplicando la sustitución s al
-- término t. Por ejemplo, 
--    λ> let s = [(("x",2), T "f" [V ("x",2), T "g" [V ("x",2)]])] ::Sustitucion
--    λ> let t = T "g" [V ("x",2)] :: Termino
--    λ> aplicaTerm s t
--    T "g" [T "f" [V ("x",2),T "g" [V ("x",2)]]]
--    λ> aplicaTerm s (T "g" [V ("x",3)])
--    T "g" [V ("x",3)]
--    λ> aplicaTerm s (T "g" [])
--    T "g" []
aplicaTerm :: Sustitucion -> Termino -> Termino
aplicaTerm s (V a)  = aplicaSust s a
aplicaTerm s (T f ts) = 
    T f (map (aplicaTerm s) ts)

-- (contenidoVar v t) se verifica si la variable v ocurre en el término
-- t. Por ejemplo,
--    λ> contenidoVar ("x",2) (V ("x",2)) 
--    True
--    λ> contenidoVar ("x",3) (V ("x",2)) 
--    False
--    λ> contenidoVar ("x",2) (T "f" [V ("x",2), T "g" [V ("x",2)]])
--    True
--    λ> contenidoVar ("y",5) (T "f" [V ("x",2), T "g" [V ("x",2)]])
--    False
contenidoVar :: Nvariable -> Termino -> Bool
contenidoVar a (V x)    = a == x
contenidoVar a (T _ ts) = any (contenidoVar a) ts

-- El problema de la unificación [Algoritmo de Martelli]
-- =====================================================

-- (resuelve ts s) es la sustitución resultante para el problema de
-- unificación ts, con la sustitución s.
--    λ> resuelve [(V ("x",2), V ("x",2))] []
--    Right []
--    λ> resuelve [(V ("x",2), V ("x",3))] []
--    Right [(("x",2),V ("x",3))]
--    λ> resuelve [(V ("x",2), V ("x",3))] [(("x",2), V ("x",3))]
--    Right [(("x",2),V ("x",3)),(("x",2),V ("x",3))]
--    λ> resuelve [(V ("x",2), T "f" [V ("x",2)])] [(("x",2), V ("x",3))]
--    Left UNIFICACION
--    λ> resuelve [(V ("x",2),T "f" [V ("x",3)]),(V ("x",3),T "f" [V ("x",4)])] []
--    Right [(("x",3),T "f" [V ("x",4)]),(("x",2),T "f" [T "f" [V ("x",4)]])]
--    λ> resuelve [(V ("x",2),T "f" [V ("x",3)]),(V ("x",3),T "f" [V ("x",4)])] [(("x",2),T "f" [V ("x",3)])]
--    Right [(("x",3),T "f" [V ("x",4)]),(("x",2),T "f" [T "f" [V ("x",4)]]),(("x",2),T "f" [T "f" [V ("x",4)]])]
--    λ> resuelve [(V ("x",1), T "f" [V ("a",1)]),(T "g" [V ("x",1),V ("x",1)], T "g" [V ("x",1),V ("y",1)])] []
--    Right [(("y",1),T "f" [V ("a",1)]),(("x",1),T "f" [V ("a",1)])]
resuelve :: [(Termino,Termino)] -> Sustitucion -> Either ERROR Sustitucion
resuelve [] s = Right s
resuelve ((V x,t):ts) s 
    | V x == t  = resuelve ts s
    | otherwise = reglaElimina x t ts s
resuelve ((t,V x):ts) s = 
    reglaElimina x t ts s
resuelve ((T f ts1, T g ts2):ts) s 
    | f == g    = resuelve (zip ts1 ts2 ++ ts) s
    | otherwise = Left UNIFICACION

-- (reglaElimina x t ts s) aplica la regla de eliminación x=^? t al
-- problema de unificación ts y sustitución s.
--    λ> reglaElimina ("x",1) (V ("x",2)) [(V ("x",1), V("y",1))] []
--    Right [(("x",2),V ("y",1)),(("x",1),V ("y",1))]
--    λ> reglaElimina ("x",1) (V ("x",2)) [(V ("x",10), V("y",1))] []
--    Right [(("x",10),V ("y",1)),(("x",1),V ("x",2))]
--    λ> reglaElimina ("x",1) (T "f" [V ("a",1)]) [(T "g" [V ("x",1),V ("x",1)], T "g" [V ("x",1),V ("y",1)])] []
--    Right [(("y",1),T "f" [V ("a",1)]),(("x",1),T "f" [V ("a",1)])]
reglaElimina :: Nvariable -> Termino -> [(Termino,Termino)] -> Sustitucion 
                -> Either ERROR Sustitucion
reglaElimina x t ts s 
    | contenidoVar x t = Left UNIFICACION
    | otherwise        = resuelve nuevaLista nuevaSustitucion
    where nuevaLista = map (\(t1,t2) ->
                                (aplicaTerm [(x,t)] t1, 
                                 aplicaTerm [(x,t)] t2)) 
                           ts
          nuevaSustitucion = (x,t) : 
                             map (\(y,u) -> (y, aplicaTerm[(x,t)] u)) s

-- (unificaEjem t1 t2) es una simplificación de la función
-- resuelve con el problema de unificación (t1 = t2) y con la
-- sustitución igual a la identidad.
--    λ> unificaEjem (T "f" [V ("x",2)]) (V("x",2))
--    Left UNIFICACION
--    λ> unificaEjem (T "f" [V ("x",3)]) (V("x",2))
--    Right [(("x",2),T "f" [V ("x",3)])]
unificaEjem :: Termino -> Termino -> Either ERROR Sustitucion
unificaEjem t1 t2 = resuelve [(t1,t2)] []

-- El problema del emparejamiento
-- ==============================

-- (empareja ts s) es la solución del problema de emparejamiento de
-- ts.
--    λ> empareja [(T "f" [V ("x",1), V("y",1)], T "f" [V ("x",1), V("z",1)])] []
--    Right [(("y",1),V ("z",1)),(("x",1),V ("x",1))]
--    λ> empareja [(T "f" [V ("x",1), V("y",1)], T "f" [T "g" [V ("z",1)], V("x",1)])] []
--    Right [(("y",1),V ("x",1)),(("x",1),T "g" [V ("z",1)])]
empareja :: [(Termino, Termino)] -> Sustitucion -> Either ERROR Sustitucion
empareja [] s = Right s
empareja ((V x,t):ts) s 
    | contenido x s = if aplicaSust s x == t
                      then empareja ts s
                      else Left UNIFICACION
    | otherwise     = empareja ts ((x,t):s)
empareja ((_,V _):_) _ = Left UNIFICACION
empareja ((T f ts1, T g ts2):ts) s 
    | f == g    = empareja ((zip ts1 ts2) ++ ts) s
    | otherwise = Left UNIFICACION
    
-- (emparejaEjem a b) es el problema de emparejamiento de la
-- inecuación a <_~^? b. Por ejemplo,
--    λ> emparejaEjem (T "f" [V ("x",3)]) (V("x",2))
--    Left UNIFICACION
--    λ> emparejaEjem (V ("x",3)) (V("x",2))
--    Right [(("x",3),V ("x",2))]
emparejaEjem :: Termino -> Termino -> Either ERROR Sustitucion
emparejaEjem a b = empareja [(a,b)] []

-- Reescritura de términos
-- =======================

-- (reescribe s t) es la primera regla de la lista s, que se puede
-- aplicar a t. Por ejemplo,
--    λ> reescribe [(V ("x",1), V ("y",1)),(V ("x",2), V ("y",2))] (V("x",2))
--    Right (V ("y",2))
--    λ> reescribe [(V ("x",1), V ("y",1)),(V ("x",1), V ("y",2))] (V("x",2))
--    Left REGLA
--    λ> reescribe [(V ("x",2),T "f" [V ("x",3)]),(V ("x",3),T "f" [V ("x",4)])] (V("x",2))
--    Right (T "f" [V ("x",3)])
reescribe :: [(Termino, Termino)] -> Termino -> Either ERROR Termino
reescribe [] _ = Left REGLA
reescribe ((l,r):ts) t
    | a == Left UNIFICACION = reescribe ts t
    | b == t = reescribe ts t
    | otherwise = Right b
    where a = emparejaEjem l r
          b = aplicaTerm (elimR a) t
          elimR (Right r) = r

-- (formaNormal s t) es la forma normal de t respecto de s.
--    λ> formaNormal [(V ("x",1), V ("y",1)),(V ("x",2), V ("y",2))] (V("x",2))
--    V ("x",2)
--    λ> formaNormal [(V ("z",1), V ("a",1)),(V ("x",1), T "g" [V ("z",1)])] (T "f" [V ("x",1)])
--    T "f" [T "g" [V ("a",1)]]
formaNormal :: [(Termino, Termino)] -> Termino -> Termino
formaNormal _ (V a) = V a
formaNormal s (T f ts)
    | a == Left REGLA = u
    | otherwise = formaNormal s (elimR a)
     where u = T f (map (formaNormal s) ts)
           a = reescribe s u
           elimR (Right r) = r

