-- Unificacion.hs
-- Unificación de términos de primer orden.
-- José A. Alonso Jiménez <jalonso@us,es>
-- Sevilla, 26 de Diciembre de 2007
-- ---------------------------------------------------------------------

import Data.List
import Data.Either

-- ---------------------------------------------------------------------
-- Variables, términos y átomos                                       --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1: Definir el tipo de datos Nombre para representar las
-- cadenas. 
-- ---------------------------------------------------------------------

type Nombre   = String

-- ---------------------------------------------------------------------
-- Ejercicio 2: Definir el tipo de datos Términos para representar los
-- términos como una variable o un símbolo de función seguido de una
-- lista de términos.  
-- ---------------------------------------------------------------------

data Término = V Nombre
             | T Nombre [Término]
             deriving Eq

instance Show Término where 
    show (V nombre)    = nombre
    show (T nombre []) = nombre
    show (T nombre ts) = nombre ++ concat [show ts]

-- ---------------------------------------------------------------------
-- Ejercicio 3: Definir el tipo de datos Atomo para representar los
-- átomos como un símbolo de predicado seguido de una lista de términos. 
-- ---------------------------------------------------------------------

data Atomo = A Nombre [Término]
             deriving Eq

instance Show Atomo where 
    show (A nombre []) = nombre
    show (A nombre ts) = nombre ++ concat [show ts]

-- ---------------------------------------------------------------------
-- Ejercicio 4: Definir el tipo de datos Variable.
-- ---------------------------------------------------------------------

type Variable = Término

-- Ejemplos de variables:
x = V "x"
y = V "y"
z = V "z"
u = V "u"
w = V "w"

-- Ejemplos de constantes:
a = T "a" []
b = T "b" []
c = T "c" []

-- Ejemplos de símbolos de función:
f = T "f"
g = T "g"
h = T "h"

-- Ejemplo de símbolos de predicado:
p = A "p"

-- ---------------------------------------------------------------------
-- Ejercicio 5: Definir la función
--    esVariable :: Término -> Bool
-- tal que (esVariable t) que se verifica si t es una variable. Por
-- ejemplo, 
--    esVariable x  ==>  True
--    esVariable a  ==>  False
-- ---------------------------------------------------------------------

esVariable :: Término -> Bool
esVariable (V _) = True
esVariable _     = False

-- ---------------------------------------------------------------------
-- Ejercicio 6: Definir la función
--    variables :: Término -> [Variable]
-- tal que (variables t) es la lista de variables de t. Por ejemplo,
--    variables (g[f[x,y],z])  ==>  [x,y,z]
-- ---------------------------------------------------------------------

variables :: Término -> [Variable]
variables (V v)    = [V v]
variables (T n ts) = variablesEnLista ts

-- ---------------------------------------------------------------------
-- Ejercicio 7: Definir la función
--    variablesEnLista :: [Término] -> [Variable]
-- tal que (variablesEnLista ts) es la lista de variables de la lista de
-- términos ts. Por ejemplo,
--    variablesEnLista [f[x,y], g[f[x,y],z]]  ==>  [x,y,z]
-- ---------------------------------------------------------------------

variablesEnLista :: [Término] -> [Variable]
variablesEnLista = nub . concat . map variables

-- ---------------------------------------------------------------------
-- Sustituciones                                                      --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 8: Definir el tipo de datos Sustitución para representar
-- las listas de pares formados por una variable y un término. 
-- ---------------------------------------------------------------------

type Sustitución = [(Variable,Término)]

-- Ejemplos de sustitución:
sigma1 :: Sustitución
sigma1 = [(x,a),(z,f[x,y])]
sigma2 = [(x,z),(y,u)]
sigma3 = [(z,x),(x,b),(u,c)]
sigma4 = [(u,f[x]),(y,a)] 
sigma5 = [(x,h[z]),(y,g[b])]

-- ---------------------------------------------------------------------
-- Ejercicio 9: Definir la función
--    epsilon :: Sustitución
-- tal que (epsilon s) es la sustitución identidad. 
-- ---------------------------------------------------------------------

epsilon :: Sustitución
epsilon = []

-- ---------------------------------------------------------------------
-- Ejercicio 10: Definir la función
--    dominio :: Sustitución -> [Variable]
-- tal que (dominio s) es el domiinio de la sustitución s. Por ejemplo,
--    dominio sigma1  ==>  [x,z]
-- ---------------------------------------------------------------------

dominio :: Sustitución -> [Variable]
dominio = map fst

-- ---------------------------------------------------------------------
-- Ejercicio 11: Definir la función
--    aplicaVar :: Sustitución -> Variable -> Término
-- tal que (aplicaVar s x) es el resultado de aplicar la sustitución s a
-- la variable x. Por ejemplo,
--    sigma1              ==>  [(x,a),(z,f[x,y])]
--    aplicaVar sigma1 x  ==>  a
--    aplicaVar sigma1 y  ==>  y
--    aplicaVar sigma1 z  ==>  f[x,y]
-- ---------------------------------------------------------------------

aplicaVar :: Sustitución -> Variable -> Término
aplicaVar []         (V y) = V y
aplicaVar ((x,t):xs) (V y) 
    | x == (V y) = t
    | otherwise  = aplicaVar xs (V y) 

-- ---------------------------------------------------------------------
-- Ejercicio 12: Definir la función
--    aplicaT :: Sustitución -> Término -> Término
-- tal que (aplicaT s t) es el resultado de aplicar la sustitución s al
-- término t. Por ejemplo,
--    sigma1                        ==>  [(x,a),(z,f[x,y])]
--    aplicaT sigma1 (g[f[x,y],z])  ==>  g[f[a,y],f[x,y]]
-- ---------------------------------------------------------------------

aplicaT :: Sustitución -> Término -> Término
aplicaT s (V x)    = aplicaVar s (V x)
aplicaT s (T n ts) = T n [aplicaT s t | t <- ts]

-- ---------------------------------------------------------------------
-- Ejercicio 13: Definir la función
--    reduce :: Sustitución -> Sustitución
-- tal que (reduce s) es la sustitución obtenida eliminando los pares de
-- la sustitución s cuyos elementos son iguales. Por ejemplo,
--    reduce [(x,a),(y,y),(z,f[x,y])]  ==>  [(x,a),(z,f[x,y])]
-- ---------------------------------------------------------------------

reduce :: Sustitución -> Sustitución
reduce s =
    [(x,t) | (x,t) <- s, x /= t]

-- ---------------------------------------------------------------------
-- Ejercicio 14: Definir la función
--    composición :: Sustitución -> Sustitución -> Sustitución 
-- tal que (composición s1 s2) es la composición de las sustituciones s1
-- y s2. Por ejemplo,
--    (sigma2,sigma3)           => ([(x,z),(y,u)],[(z,x),(x,b),(u,c)])
--    composición sigma2 sigma3 => [(y,c),(z,x),(u,c)]
--    (sigma4, sigma5)          => ([(u,f[x]),(y,a)],[(x,h[z]),(y,g[b])])
--    composición sigma4 sigma5 => [(u,f[h[z]]),(y,a),(x,h[z])]
-- ---------------------------------------------------------------------

composición :: Sustitución -> Sustitución -> Sustitución 
composición xs ys = 
    (reduce [ (y,(aplicaT ys t)) | (y,t) <-  xs ])
    ++
    [ (x,t) | (x,t) <- ys, x `notElem` (dominio xs) ]

-- ---------------------------------------------------------------------
-- Unificación                                                        --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 15: Definir la función
--    unifica :: Término -> Término -> [Sustitución]
-- tal que (unifica t1 t2) es la lista formada por un unificador de
-- máxima generalidad de los términos t1 y t2, si son unificables y es
-- la lista vacía en caso contrario, Por ejemplo,
--    unifica a a                   ==>  [[]]
--    unifica x a                   ==>  [[(x,a)]]
--    unifica x (f[y])              ==>  [[(x,f[y])]]
--    unifica x (f[x])              ==>  []
--    unifica (f[y]) x              ==>  [[(x,f[y])]]
--    unifica (f[x]) x              ==>  []
--    unifica a b                   ==>  []
--    unifica (f[x,b]) (f[a,y])     ==>  [[(y,b),(x,a)]]
--    unifica (f[x,x]) (f[a,b])     ==>  []
--    unifica (f[x,g[y]]) (f[y,x])  ==>  []
-- ---------------------------------------------------------------------

unifica :: Término -> Término -> [Sustitución]
unifica (V x) (V y) 
    | x==y      = [epsilon] 
    | otherwise = [[(V x,V y)]]
unifica (V x) t2 = 
    [ [(V x,t2)] | (V x) `notElem` variables t2 ]
unifica t1 (V y) = 
    [ [(V y,t1)] | (V y) `notElem` variables t1 ]
unifica (T f ts) (T g rs) = 
    [ u | f==g, u <- unificaListas ts rs ]

-- ---------------------------------------------------------------------
-- Ejercicio 16: Definir la función
--    unificaListas :: [Término] -> [Término] -> [Sustitución]
-- tal que (unificaListas (unificaListas ts1 ts2) es la lista formada
-- por un unificador de máxima generalidad de las ecuaciones
-- correspondientes a las listas de términos ts1 y ts2, si son
-- unificables y es la lista vacía en caso contrario. Por ejemplo,
--    unificaListas [x,f[x],y] [a,y,z] => [[(z,f[a]),(y,f[a]),(x,a)]]
--    unificaListas [x,f[x]] [y,y]     => []
-- ---------------------------------------------------------------------

unificaListas :: [Término] -> [Término] -> [Sustitución]
unificaListas []     []     = [epsilon]
unificaListas []     (r:rs) = []
unificaListas (t:ts) []     = []
unificaListas (t:ts) (r:rs) = 
   [ composición sigma2 sigma1 
     | sigma1 <- unifica t r,
       sigma2 <- unificaListas [aplicaT sigma1 t | t <- ts] 
                               [aplicaT sigma1 r | r <- rs] ]

-- ---------------------------------------------------------------------
-- Ejercicio 17: Definir la función
--    unificaA :: Atomo -> Atomo -> [Sustitución]
-- tal que (unificaA a1 a2) es la lista formada por un unificador de
-- máxima generalidad de los átomos a1 y a2, si son unificables y es la
-- lista vacía en caso contrario, Por ejemplo,
--    unificaA (p[w,a,h[w]]) (p[f[x,y],x,z])
--    ==> [[(z,h[f[a,y]]),(x,a),(w,f[x,y])]]
--    unificaA (p[w,a,h[w]]) (p[f[x,y],x,y])
--    ==> []
-- ---------------------------------------------------------------------

unificaA :: Atomo -> Atomo -> [Sustitución]
unificaA (A n1 ts1) (A n2 ts2) 
    | n1==n2    = unificaListas ts1 ts2
    | otherwise = []

