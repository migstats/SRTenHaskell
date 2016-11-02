-- Terminos.hs
-- Términos de primer orden.
-- Sevilla, 18 de Mayo de 2016
-- ---------------------------------------------------------------------

module Terminos where

import Data.Either hiding (isRight)
import Debug.Trace
import Data.List (union)

-- Tengo una versión del paquete Data.Either anterior a 4.7.0.0, que es
-- la version en la que se introdujo isRight. He intentado usar cabal
-- update, y cabal install pero no he conseguido actualizarla todavía.
isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True
    
-- ---------------------------------------------------------------------
-- § El tipo de los términos                                          --
-- ---------------------------------------------------------------------

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
--    ghci> :t (V ("x",2))
--    (V ("x",2)) :: Termino
--    ghci> :t (T "a" [])
--    (T "a" []) :: Termino
--    ghci> :t (T "f" [V ("x",2), T "g" [V ("x",2)]])
--    (T "f" [V ("x",2), T "g" [V ("x",2)]]) :: Termino
data Termino = V Nvariable
             | T String [Termino]
             deriving (Eq, Show)

-- ---------------------------------------------------------------------
-- § Propiedades de términos                                          --
-- ---------------------------------------------------------------------

-- (ocurre v t) se verifica si la variable v ocurre en el término
-- t. Por ejemplo,
--    ghci> ocurre ("x",2) (V ("x",2)) 
--    True
--    ghci> ocurre ("x",3) (V ("x",2)) 
--    False
--    ghci> ocurre ("x",2) (T "f" [V ("x",2), T "g" [V ("x",2)]])
--    True
--    ghci> ocurre ("y",5) (T "f" [V ("x",2), T "g" [V ("x",2)]])
--    False
ocurre :: Nvariable -> Termino -> Bool
ocurre a (V x)    = a == x
ocurre a (T _ ts) = any (ocurre a) ts

-- (conjuntoVar t) es el conjunto de variables de un término t. Por
-- ejemplo,
--    ghci> conjuntoVar (V ("x",2))
--    [("x",2)]
--    ghci> conjuntoVar (T "f" [T "e" []])
--    []
--    ghci> conjuntoVar (T "f" [T "g" [V ("x",1)], T "h" [], V ("y",1)])
--    [("x",1),("y",1)]
conjuntoVar :: Termino -> [Nvariable]
conjuntoVar (V x)    = [x]
conjuntoVar (T _ ts) = concatMap conjuntoVar ts

-- (longitudTerm t) es la longitud del término t. Por ejemplo,
--    ghci> longitudTerm (V ("x",2))
--    1
--    ghci> longitudTerm (T "f" [T "e" []])
--    2
--    ghci> longitudTerm (T "f" [T "g" [V ("x",1)], T "h" [], V ("y",1)])
--    5
longitudTerm :: Termino -> Int
longitudTerm (V _)    = 1
longitudTerm (T _ xs) = 1 + sum (map longitudTerm xs)

-- (tamanoTerm t) es el tamaño del término t. Por ejemplo,
--    ghci> tamanoTerm (V ("x",2))
--    0
--    ghci> tamanoTerm (T "f" [T "e" []])
--    1
--    ghci> tamanoTerm (T "f" [T "g" [V ("x",1)], T "h" [], V ("y",1)])
--    2
tamanoTerm :: Termino -> Int
tamanoTerm (V _)    = 0
tamanoTerm (T _ []) = 0
tamanoTerm (T _ xs) = 1 + sum (map tamanoTerm xs)

-- (conjuntoPos t) es el conjunto Pos(t) de un término t. Por ejemplo,
--    ghci> conjuntoPos (V ("x",1))
--    [[]]
--    ghci> conjuntoPos (T "f" [V("x",1)])
--    [[],[1]]
--    ghci> conjuntoPos (T "f" [T "i" [T "i" [T "e" []]], T "f" [T "i" [V("x",1)], V("x",1)]])
--    [[],[1],[1,1],[1,1,1],[2],[2,1],[2,1,1],[2,2]]
conjuntoPos :: Termino -> [[Int]]
conjuntoPos (V _) = [[]]
conjuntoPos (T _ (xs)) = conjuntoPos' (reverse zs)
    where zs = (zip xs [1..])
          conjuntoPos' [] = [[]]
          conjuntoPos' ((t,n):ts) = (conjuntoPos' ts) ++ (map (n:) (conjuntoPos t))

-- (esSubtermino t s) verifica si t es subtérmino de s. Por ejemplo,
--    ghci> esSubtermino (V ("x",1)) (T "f" [V("x",1)])
--    True
--    ghci> esSubtermino (V ("x",2)) (T "f" [V("x",1)])
--    False
esSubtermino :: Termino -> Termino -> Bool
esSubtermino t s@(T _ ts)
    = t == s || any (esSubtermino t) ts
esSubtermino t s = t == s

-- (sustPosSubtermino s t p) es el término s[t]_p. Por ejemplo,
--    ghci> sustPosSubtermino (T "f" [V ("s",1),V ("s",2),V ("s",3)]) (V ("t",1)) [2,1]
--    T "f" [V ("s",1),V ("t",1),V ("s",3)]
--    sustPosSubtermino (T "f" [V ("s",1),V ("s",2),V ("s",3)]) (V ("t",1)) []
--    (V ("t",1))
--    sustPosSubtermino (V ("s",1)) (V ("t",1)) [2]
--    *** Exception: No se ha definido bien la lista de posiciones
--    ghci> sustPosSubtermino (T "f" [V ("s",1),V("s",2),V("s",3)]) (V ("t",1)) [2,2]
--    T "f" [V ("s",1),*** Exception: No se ha definido bien la lista de posiciones
sustPosSubtermino :: Termino -> Termino -> [Int] -> Termino
sustPosSubtermino _ t [] = t
sustPosSubtermino (V _) t [1] = t
sustPosSubtermino (V _) _ _ =
  error "No se ha definido bien la lista de posiciones"
sustPosSubtermino (T f xs) t (i:is) =
  T f (take (i-1) xs ++
       (sustPosSubtermino (xs!!(i-1)) t is) : (drop i xs))

-- ---------------------------------------------------------------------
-- § Sustituciones                                                    --
-- ---------------------------------------------------------------------

-- Una sustitución es una lista de pares formados por variables y
-- términos. Por ejemplo,
--    ghci> [(("x",2),T "a" [])] :: Sustitucion
--    [(("x",2),T "a" [])]
--    ghci> [(("x",2),T "a" []),(("y",4),(V ("z",5)))] :: Sustitucion
--    [(("x",2),T "a" []),(("y",4),V ("z",5))]
type Sustitucion = [(Nvariable,Termino)]

-- (enDominio v s) se verifica si la variable v está en el dominio de la
-- sustitución s. Por ejemplo,
--    ghci> let s = [(("x",2),T "a" []),(("y",4),(V ("z",5)))]
--    ghci> enDominio ("x",2) s
--    True
--    ghci> enDominio ("x",3) s
--    False
--    ghci> enDominio ("y",2) s
--    False
--    ghci> enDominio ("y",4) s
--    True
--    ghci> enDominio ("z",5) s
--    False
enDominio :: Nvariable -> Sustitucion -> Bool
enDominio v = any (\(x,_) -> v == x)

-- (aplicaVar s v) es el término obtenido aplicando la sustitución s a la
-- variable v. Por ejemplo,
--    ghci> let s = [(("x",2),T "a" []),(("y",4),(V ("z",5)))]
--    ghci> aplicaVar s ("x",2)
--    T "a" []
--    ghci> aplicaVar s ("x",3)
--    V ("x",3)
--    ghci> aplicaVar s ("y",2)
--    V ("y",2)
--    ghci> aplicaVar s ("y",4)
--    V ("z",5)
--    ghci> aplicaVar s ("z",5)
--    V ("z",5)
aplicaVar :: Sustitucion -> Nvariable -> Termino 
aplicaVar [] z = V z 
aplicaVar ((x,y):s) z 
  | x == z    = y
  | otherwise = aplicaVar s z

-- (aplicaTerm s t) es el término obtenido aplicando la sustitución s al
-- término t. Por ejemplo, 
--    ghci> let s = [(("x",2),T "a" []),(("y",4),(V ("z",5)))]
--    ghci> aplicaTerm s (T "s" [V ("x",2), T "m" [V ("y",4)]])
--    T "s" [T "a" [],T "m" [V ("z",5)]]
--    ghci> aplicaTerm s (T "s" [V ("x",4), T "m" [V ("y",2)]])
--    T "s" [V ("x",4),T "m" [V ("y",2)]]
aplicaTerm :: Sustitucion -> Termino -> Termino
aplicaTerm s (V x)    = aplicaVar s x
aplicaTerm s (T f ts) = T f (map (aplicaTerm s) ts)
                    
-- ---------------------------------------------------------------------
-- § Algoritmo de unificación                                         --
-- ---------------------------------------------------------------------

-- Los errores son causados por UNIFICACION (si el sistema no tiene
-- solución) o REGLA (si durante la reescritura no se puede aplicar
-- ninguna regla más)
data ERROR = UNIFICACION
           | REGLA
           deriving (Eq, Show)

-- Una ecuación es un par de términos
type Ecuacion = (Termino,Termino)

-- Un sistema de ecuaciones es una lista de ecuaciones
type Sistema = [Ecuacion]

-- (unificacion t1 t2) es 
-- + (Right s) si los términos t1 y t2 son unificables y s es un
--   unificador del máxima generalidad de t1 y t2 y
-- + (Left UNIFICACION) si t1 y t2 no son unificables.
-- Por ejemplo,
--    unificacion (T "f" [V ("x",0), T "g" [V ("z",0)]])
--                (T "f" [T "g" [V ("y",0)], V ("x",0)])
--    == Right [(("z",0),V ("y",0)),(("x",0),T "g" [V ("y",0)])]
--    unificacion (T "f" [V ("x",0),T "b" []])
--                (T "f" [T "a" [],V ("y",0)])
--    == Right [(("y",0),T "b" []),(("x",0),T "a" [])]
--    unificacion (T "f" [V ("x",0),V ("x",0)])
--                (T "f" [T "a" [],T "b" []])
--    == Left UNIFICACION
--    unificacion (T "f" [V ("x",0),T "g" [V ("y",0)]])
--                (T "f" [V ("y",0),V ("x",0)])
--    == Left UNIFICACION
unificacion :: Termino -> Termino -> Either ERROR Sustitucion
unificacion t1 t2 = unificacionS [(t1,t2)] []

-- (unificacionS es s) es
-- + (Right s') si el sistema s(es), obtenido aplicando la sustitución s
--   a la ecuaciones de es, es unificable y s' es un unificador del máxima
--   generalidad de s(es);
-- + (Left UNIFICACION) si s(es) no es unificable.
-- Por ejemplo,
--    unificacionS [(T "f" [V ("x",0), T "g" [V ("z",0)]],
--                   T "f" [T "g" [V ("y",0)], V ("x",0)])]
--                 []
--    == Right [(("z",0),V ("y",0)),(("x",0),T "g" [V ("y",0)])]
--    unificacionS [(T "f" [V ("x",0),T "b" []],
--                   T "f" [T "a" [],V ("y",0)])]
--                 []
--    == Right [(("y",0),T "b" []),(("x",0),T "a" [])]
--    unificacionS [(T "f" [V ("x",0),V ("x",0)],
--                   T "f" [T "a" [],T "b" []])]
--                 []
--    == Left UNIFICACION
--    unificacionS [(T "f" [V ("x",0),T "g" [V ("y",0)]],
--                   T "f" [V ("y",0),V ("x",0)])]
--                 []
--    == Left UNIFICACION
unificacionS :: Sistema -> Sustitucion -> Either ERROR Sustitucion
unificacionS [] s = Right s
unificacionS ((V x,t):ts) s 
  | V x == t  = unificacionS ts s
  | otherwise = reglaElimina x t ts s
unificacionS ((t,V x):ts) s = 
  reglaElimina x t ts s
unificacionS ((T f ts1, T g ts2):ts) s 
  | f == g    = unificacionS (zip ts1 ts2 ++ ts) s
  | otherwise = Left UNIFICACION

-- (reglaElimina x t es s) es
-- + (Left UNIFICACION), si x ocurre en t y
-- + la solución del sistema obtenido al aplicarle a es la sustitución
--   [x/t] en el entorno obtenido componiendo la sustitución s con
--   [x/t], en caso contrario.
-- Por ejemplo,
--    ghci> reglaElimina ("x",0) (T "f" [V ("x",0)]) [] []
--    Left UNIFICACION
--    ghci> reglaElimina ("x",0) (T "f" [V ("x",1)]) [] []
--    Right [(("x",0),T "f" [V ("x",1)])]
--    ghci> reglaElimina ("x",0) (T "f" [V ("x",1)]) [] [(("y",2),V ("x",0))]
--    Right [(("x",0),T "f" [V ("x",1)]),(("y",2),T "f" [V ("x",1)])]
--    ghci> reglaElimina ("x",0) (T "f" [V ("x",1)]) [(V ("x",1),V ("x",0))] []
--    Left UNIFICACION
--    ghci> reglaElimina ("x",0) (T "f" [V ("x",1)]) [(V ("x",1),V ("y",0))] []
--    Right [(("x",1),V ("y",0)),(("x",0),T "f" [V ("y",0)])]
--    ghci> reglaElimina ("x",0) (T "f" [V ("x",1)]) [(V ("y",1),V ("x",0))] []
--    Right [(("y",1),T "f" [V ("x",1)]),(("x",0),T "f" [V ("x",1)])]
reglaElimina :: Nvariable -> Termino -> Sistema -> Sustitucion 
             -> Either ERROR Sustitucion
reglaElimina x t es s 
  | ocurre x t = Left UNIFICACION
  | otherwise  = unificacionS es' s'
  where es' = [(aplicaTerm [(x,t)] t1, aplicaTerm [(x,t)] t2) | (t1,t2) <- es] 
        s'  = (x,t) : map (\(y,u) -> (y, aplicaTerm [(x,t)] u)) s

-- A continuación se muestra el cálculo de los ejemplos de unificación
-- del tema 12 del curso de "LMF" que se encuentra en
-- http://bit.ly/29bWIYM 

-- Para obtener los cálculos se usa la librería Trace, añadiendo al
-- principio del módulo
--    import Debug.Trace
-- y, añadiendo como primera ecuación de la definición de unificacionS la
-- siguiente 
--    unificacionS es s | trace ("unificacionS " ++ show es ++ "\n" ++
--                               "         " ++ show s)
--                              False =
--                        undefined

-- Ejemplo 1 (p. 14) Unificar f(x,g(z)) y f(g(y),x)
--    unificacion (T "f" [V ("x",0), T "g" [V ("z",0)]])
--                (T "f" [T "g" [V ("y",0)], V ("x",0)])
--    = unificacionS [(T "f" [V ("x",0), T "g" [V ("z",0)]],
--                 T "f" [T "g" [V ("y",0)], V ("x",0)])]
--               []
--    = unificacionS [(V ("x",0),T "g" [V ("y",0)]),
--                (T "g" [V ("z",0)],V ("x",0))]
--               []
--    = unificacionS [(T "g" [V ("z",0)],T "g" [V ("y",0)])]
--               [(("x",0),T "g" [V ("y",0)])]
--    = unificacionS [(V ("z",0),V ("y",0))]
--               [(("x",0),T "g" [V ("y",0)])]
--    = unificacionS []
--               [(("z",0),V ("y",0)),(("x",0),T "g" [V ("y",0)])]
--    = Right [(("z",0),V ("y",0)),(("x",0),T "g" [V ("y",0)])]
-- El UMG es [z/y,x/g(y)]   

-- Ejemplo 2 (p. 15): Unificar f(x,b) y f(a,y)
--    unificacion (T "f" [V ("x",0),T "b" []])
--                (T "f" [T "a" [],V ("y",0)])
--    = unificacionS [(T "f" [V ("x",0),T "b" []],
--                 T "f" [T "a" [],V ("y",0)])]
--               []
--    = unificacionS [(V ("x",0),T "a" []),
--                (T "b" [],V ("y",0))]
--               []
--    = unificacionS [(T "b" [],V ("y",0))]
--               [(("x",0),T "a" [])]
--    = unificacionS []
--               [(("y",0),T "b" []),(("x",0),T "a" [])]
--    = Right [(("y",0),T "b" []),(("x",0),T "a" [])]
-- El UMG es [y/b, x/a].

-- Ejemplo 3 (p. 15): Unificar f(x,x) y f(a,b)
--    unificacion (T "f" [V ("x",0),V ("x",0)])
--                (T "f" [T "a" [],T "b" []])
--    = unificacionS [(T "f" [V ("x",0),V ("x",0)],
--                 T "f" [T "a" [],T "b" []])]
--               []
--    = unificacionS [(V ("x",0),T "a" []),
--                (V ("x",0),T "b" [])]
--               []
--    = unificacionS [(T "a" [],T "b" [])]
--               [(("x",0),T "a" [])]
--    = Left UNIFICACION
-- No son unificables

-- Ejemplo 4 (p. 16): Unificar f(x,g(y)) y f(y,x)
--    unificacion (T "f" [V ("x",0),T "g" [V ("y",0)]])
--                (T "f" [V ("y",0),V ("x",0)])
--    = unificacionS [(T "f" [V ("x",0),T "g" [V ("y",0)]],
--                 T "f" [V ("y",0),V ("x",0)])]
--               []
--    = unificacionS [(V ("x",0),V ("y",0)),
--                (T "g" [V ("y",0)],V ("x",0))]
--               []
--    = unificacionS [(T "g" [V ("y",0)],V ("y",0))]
--               [(("x",0),V ("y",0))]
--    Left UNIFICACION

-- ---------------------------------------------------------------------
-- § Equiparación de términos                                         --
-- ---------------------------------------------------------------------

-- (equiparacion t1 t2) es
-- + (Right s) si t1 y es una instancia de t1 y s es un
--   equiparador del máxima generalidad de t1 con t2 y
-- + (Left UNIFICACION) en caso contrario.
-- Por ejemplo,
--    λ> let t1 = T "f" [V ("x",0), T "a" []]
--    λ> let t2 = T "f" [T "b" [],  T "a" []]
--    λ> equiparacion t1 t2
--    Right [(("x",0),T "b" [])]
--    λ> equiparacion t2 t1
--    Left UNIFICACION
--    λ> let t3 = T "f" [V ("x",0), V ("y",0)]
--    λ> equiparacion t1 t3
--    Left UNIFICACION
equiparacion :: Termino -> Termino -> Either ERROR Sustitucion
equiparacion t1 t2 = equiparacionS [(t1,t2)] []

-- (equiparacionS es s) es
-- + (Right s') si la derecha de las ecuaciones de s(es) y es una
--   instancia de la izquierda y s' es un equiparador de máxima
--   generalidad de la izquierda con la derecha,
-- + (Left UNIFICACION) en caso contrario.
-- Por ejemplo,
--    λ> let t1 = T "f" [V ("x",0), T "a" []]
--    λ> let t2 = T "f" [T "b" [],  T "a" []]
--    λ> equiparacionS [(t1,t2)] []
--    Right [(("x",0),T "b" [])]
--    λ> equiparacionS [(t2,t1)] []
--    Left UNIFICACION
--    λ> let t3 = T "f" [V ("x",0), V ("y",0)]
--    λ> equiparacionS [(t1,t3)] []
--    Left UNIFICACION
equiparacionS :: Sistema -> Sustitucion -> Either ERROR Sustitucion
equiparacionS [] s = Right s
equiparacionS ((V x,t):es) s 
    | not (enDominio x s)   = equiparacionS es ((x,t):s)
    | aplicaVar s x == t    = equiparacionS es s
    | otherwise             = Left UNIFICACION
equiparacionS ((_,V _):_) _ = Left UNIFICACION
equiparacionS ((T f ts1, T g ts2):es) s 
    | f == g    = equiparacionS (zip ts1 ts2 ++ es) s
    | otherwise = Left UNIFICACION

-- A continuación se muestras cálculos de equiparadores. Para obtener
-- los cálculos se añade como primera ecuación de la definición de
-- equiparacionS la siguiente 
--    equiparacionS es s | trace ("equiparacionS " ++ show es ++ "\n" ++
--                                "              " ++ show s)
--                               False =
--                         undefined

-- Ejemplo 1: Equiparación de f(x,a) con f(b,a)
--    equiparacion (T "f" [V ("x",0), T "a" []])
--                 (T "f" [T "b" [],  T "a" []])
--    = equiparacionS [(T "f" [V ("x",0),T "a" []],
--                      T "f" [T "b" [],T "a" []])]
--                    []
--    = equiparacionS [(V ("x",0),T "b" []),
--                     (T "a" [],T "a" [])]
--                    []
--    = equiparacionS [(T "a" [],T "a" [])]
--                    [(("x",0),T "b" [])]
--    = equiparacionS []
--                    [(("x",0),T "b" [])]
--    = Right [(("x",0),T "b" [])]
-- El equiparador es [x/b]

-- Ejemplo 2: Equiparación de f(b,a) con f(xma)
--    equiparacion (T "f" [T "b" [],T "a" []]) (T "f" [V ("x",0),T "a" []])
--    = equiparacionS [(T "f" [T "b" [],T "a" []],
--                      T "f" [V ("x",0),T "a" []])]
--                    []
--    = equiparacionS [(T "b" [],V ("x",0)),
--                     (T "a" [],T "a" [])]
--                    []
--    Left UNIFICACION

-- Ejemplo 3: Equiparación de f(x,a) con f(b,y)
--    equiparacion (T "f" [V ("x",0),T "a" []]) (T "f" [T "b" [],V ("y",0)])
--    = equiparacionS [(T "f" [V ("x",0),T "a" []],
--                      T "f" [T "b" [],V ("y",0)])]
--                    []
--    = equiparacionS [(V ("x",0),T "b" []),
--                     (T "a" [],V ("y",0))]
--                    []
--    = equiparacionS [(T "a" [],V ("y",0))]
--                    [(("x",0),T "b" [])]
--    = Left UNIFICACION
-- No es equiparable

-- ---------------------------------------------------------------------
-- § Reescritura de términos                                          --
-- ---------------------------------------------------------------------

-- (reescribe es t) es
-- + (Right s'), donde s' es el término obtenido reescribiendo t con la
--   primera regla de es que con la que se pueda reescribir.
-- + (Left REGLA) en caso contrario.
-- Por ejemplo,  
--    ghci> reescribe [(V ("x",1), V ("y",1)),
--                     (V ("x",2), V ("y",2))]
--                    (V("x",2))
--    Right (V ("y",2))
--    ghci> reescribe [(V ("x",1), V ("y",1)),
--                     (V ("x",1), V ("y",2))]
--                    (V("x",2))
--    Left REGLA
--    ghci> reescribe [(V ("x",2),T "f" [V ("x",3)]),
--                     (V ("x",3),T "f" [V ("x",4)])]
--                    (V("x",2))
--    Right (T "f" [V ("x",3)])
--    ghci> reescribe [(V ("x",1),T "a" [])]
--                    (T "f" [V ("x",1), T "b" []])
--    Right (T "f" [T "a" [],T "b" []])
reescribe :: Sistema -> Termino -> Either ERROR Termino
reescribe [] _ = Left REGLA
reescribe ((l,r):es) t
    | a == Left UNIFICACION = reescribe es t
    | b == t = reescribe es t
    | otherwise = Right b
    where a = equiparacion l r
          b = aplicaTerm (elimR a) t
          elimR (Right x) = x

-- (formaNormal es t) es la forma normal de t respecto de es. Por
-- ejemplo, 
--    ghci> formaNormal [(V ("x",1), V ("y",1)),
--                       (V ("x",2), V ("y",2))]
--                      (V("x",2))
--    V ("y",2)
--    ghci> formaNormal [(V ("x",1), V ("y",1)),
--                       (V ("x",2), V ("y",2))]
--                      (V("x",3))
--    V ("x",3)
--    ghci> formaNormal [(V ("z",1), V ("a",1)),
--                       (V ("x",1), T "g" [V ("z",1)])]
--                      (T "f" [V ("x",1)])
--    T "f" [T "g" [V ("a",1)]]
formaNormal :: Sistema -> Termino -> Termino
formaNormal es (V x)
  | isRight a = elimR a
  | otherwise = V x
  where a = reescribe es (V x)
        elimR (Right r) = r
formaNormal es (T f ts)
  | a == Left REGLA = u
  | otherwise       = formaNormal es (elimR a)
  where u = T f (map (formaNormal es) ts)
        a = reescribe es u
        elimR (Right r) = r

