# Sistemas de reescritura desde el punto de vista de la programación
  funcional

Trabajo de fin de grado sobre los sistemas de reescritura tutorizado
por [Jose A. Alonso](https://twitter.com/Jose_A_Alonso) y María José
Hidalgo.

## Breve introducción

Supongamos que tenemos la ecuación $2x - 2 = 0$. Para resolverla
utilizaremos varias reglas, para simplificar tomaremos dos; `Dividir
por constante` y `Despejar`. La primera divide los dos miembros de la
ecuación por la constante que multiplica a $x$, la segunda despeja
constantes al miembro derecho. A continuación, tenemos dos maneras de
proceder,

+ $2x - 2 = 0$ ~ `Despejar` ~ $2x = 2$ ~ `Dividir por constante` ~ $x
= 1$

+ $2x - 2 = 0$ ~ `Dividir por constante` ~ $x - 1 = 0$ ~ `Despejar` ~
$x = 1$

Como hemos observamos, ambos **terminan** (pues no podemos aplicar
ninguna regla más) y además en el **mismo resultado**. Sin embargo
esto no siempre puede ocurrir. Si entre nuestras reglas tuvieramos
`Despejar hacia Izq`,

+ $2x - 2 = 0$ ~ `Despejar` ~ $2x = 2$ ~ `Despejar hacia Izq` ~ $2x -
2 = 0$ $ ~ `Despejar` $2x = 2$ ...

Y si añadieramos la regla `Acaba con cero` que transforma una ecuación
sin resolver a $x= 0$, el orden a la hora de aplicar es fundamental,
pues dependiendo de las reglas que elijamos, podremos acabar en $x =
0$ ó en $x = 1$.

Por ello, debemos formalizar que es un elemento al que se le pueden
aplicar reglas, que reglas podemos tener, y que caracteristicas tienen
esos elemenos con esas reglas.

## Rudimentos necesarios

En los capítulos 1, 2 y 3 introduciremos álgebra de relaciones de
elementos, términos (los elementos que anteriormente hablamos),
sistemas de reescritura y unificación.

## Problemas a resolver

En los capítulos 4, 5, 6 nos centraremos en estudiar propiedades de
los sistemas reescritura, en concreto sobre cuando terminan, si todos
terminan en el mismo término, y si podemos realizar alguna
modificación a las reglas para que las anteriores propiedades se
verifiquen.

## Programación

Los algorítmos que se estudian durante este trabajo serán presentados
en `Haskell`, siguiendo el código `OCaml` del libro *Term rewriting
and all that ~F. Baader T. Nipkow*.

## Feedback

Cualquier duda, sugerencia o errata es bienvenida, puedes ponerte en contacto comingo mediante Github o via [email](mailto:youremailaddress)