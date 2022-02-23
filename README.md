# Analisis de lenguajes de programación.

# Trabajo práctico final.

# Funciones recursivas de lista.

Se trata de un EDSL que toma listas de enteros y operaciones sobre las mismas y las computa hasta llegar a un estado final, mostrado la lista.

Listas de enteros:
[1,2,3,4]

Lista vacia:
[]

Operadores:

Cero a izquierda: Oi [x1, x2, . . . , xk ] = [0, x1, x2, . . . , xk ]
Cero a derecha: Od [x1, x2, . . . , xk ] = [x1, x2, . . . , xk , 0]
Borrar a izquierda: Di [x1, x2, . . . , xk ] = [x2, x3, . . . , xk ]
Borrar a derecha: Dd [x1, x2, . . . , xk ] = [x1, x2, . . . , xk−1]
Sucesor a izquierda: Si [x1, x2, . . . , xk ] = [x1 + 1, x2, . . . , xk ]
Sucesor a derecha: Sd [x1, x2, . . . , xk ] = [x1, x2, . . . , xk + 1]

<F>[x, Y , z] = {
    [x, Y , z]          si x == z
    F<F>[x, Y , z]      si x != z
}


lista ::= lista
        | Oi lista
        | Od lista
        | Di lista
        | Dd lista
        | Si lista
        | Sd lista
        | <fun> lista

fun ::= DEF lista = lista