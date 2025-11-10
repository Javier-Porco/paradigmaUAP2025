module Clase4 exposing (..)

{-| Ejercicios de Programación Funcional - Clase 4
Este módulo contiene ejercicios para practicar pattern matching y mónadas en Elm
usando árboles binarios como estructura de datos principal.

Temas:
- Pattern Matching con tipos algebraicos
- Mónada Maybe para operaciones opcionales
- Mónada Result para manejo de errores
- Composición monádica con andThen
-}
import Html exposing (a)


-- ============================================================================
-- DEFINICIÓN DEL ÁRBOL BINARIO
-- ============================================================================

type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


-- ============================================================================
-- PARTE 0: CONSTRUCCIÓN DE ÁRBOLES
-- ============================================================================


-- 1. Crear Árboles de Ejemplo


arbolVacio : Tree Int
arbolVacio =
    Empty


arbolHoja : Tree Int
arbolHoja =
    Node 5 Empty Empty


arbolPequeno : Tree Int
arbolPequeno =
    Node 3
        (Node 1 Empty Empty)
        (Node 5 Empty Empty)


arbolMediano : Tree Int
arbolMediano =
    Node 10
        (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty))
        (Node 15 (Node 12 Empty Empty) (Node 20 Empty Empty))


-- 2. Es Vacío


esVacio : Tree a -> Bool
esVacio arbol =
    case arbol of
        Empty ->
            True
        Node _ _ _ ->
            False

-- 3. Es Hoja


esHoja : Tree a -> Bool
esHoja arbol =
    case arbol of
        Node _ Empty Empty ->
            True
        _ ->
            False


-- ============================================================================
-- PARTE 1: PATTERN MATCHING CON ÁRBOLES
-- ============================================================================


-- 4. Tamaño del Árbol


tamaño : Tree a -> Int
tamaño arbol =
    case arbol of
        Empty ->
            0
        Node _ izquierdo derecho ->
            1 + tamaño izquierdo + tamaño derecho


-- 5. Altura del Árbol


altura : Tree a -> Int
altura arbol =
    case arbol of
        Empty ->
            0
        Node _ izquierda derecha ->
            1 + max (altura izquierda) (altura derecha) -- usamos max para obtener la altura mayor



-- 6. Suma de Valores


sumarArbol : Tree Int -> Int
sumarArbol arbol =
    case arbol of
        Empty ->
            0
        Node a izquierda derecha ->
            a + sumarArbol izquierda + sumarArbol derecha


-- 7. Contiene Valor


contiene : a -> Tree a -> Bool
contiene valor arbol =
    case arbol of
        Empty ->
            False
        Node valorActual izquierda derecha ->
            valor == valorActual || contiene valor izquierda || contiene valor derecha


-- 8. Contar Hojas


contarHojas : Tree a -> Int
contarHojas arbol =
    case arbol of
        Empty ->
            0
        Node _ Empty Empty ->
            1 -- cuando los nodos sean hojas van a valer 1 asi luego se suman en el ultimo caso
        Node _ izquierda derecha ->
            contarHojas izquierda + contarHojas derecha



-- 9. Valor Mínimo (sin Maybe)


minimo : Tree Int -> Int
minimo arbol =
    case arbol of
        Empty ->
            0

        Node valor izq der ->
            let
                minIzq = if esVacio izq then valor else minimo izq
                minDer = if esVacio der then valor else minimo der
            in
            min valor (min minIzq minDer)


-- 10. Valor Máximo (sin Maybe)


maximo : Tree Int -> Int
maximo arbol =
    case arbol of
        Empty ->
            0

        Node valor izq der ->
            let
                maxIzq = if esVacio izq then valor else maximo izq
                maxDer = if esVacio der then valor else maximo der
            in
            max valor (max maxIzq maxDer)


-- ============================================================================
-- PARTE 2: INTRODUCCIÓN A MAYBE
-- ============================================================================


-- 11. Buscar Valor


-- 11. Buscar Valor
buscar : a -> Tree a -> Maybe a
buscar valorBuscar arbol =
    case arbol of
        Empty ->
            Nothing

        Node valorActual subarbolIzquierdo subarbolDerecho ->
            -- ¿Es este el valor que buscamos?
            if valorActual == valorBuscar then
                Just valorActual

            else
                let
                    resultadoIzquierda = buscar valorBuscar subarbolIzquierdo
                in
                case resultadoIzquierda of
                    Just valorEncontrado ->
                        Just valorEncontrado

                    Nothing ->
                        buscar valorBuscar subarbolDerecho

-- 12. Encontrar Mínimo (con Maybe)


encontrarMinimo : Tree comparable -> Maybe comparable
encontrarMinimo arbol =
    case arbol of
        Empty ->
            Nothing

        Node valor Empty Empty ->
            Just valor

        Node valor izq der ->
            let
                minIzq = case encontrarMinimo izq of
                    Just v -> v
                    Nothing -> valor

                minDer = case encontrarMinimo der of
                    Just v -> v
                    Nothing -> valor
            in
            Just (min valor (min minIzq minDer))
            


-- 13. Encontrar Máximo (con Maybe)


encontrarMaximo : Tree comparable -> Maybe comparable
encontrarMaximo arbol =
    case arbol of
        Empty ->
            Nothing

        Node valor Empty Empty ->
            Just valor

        Node valor izq der ->
            let
                maxIzq =
                    case encontrarMaximo izq of
                        Just v -> v
                        Nothing -> valor

                maxDer =
                    case encontrarMaximo der of
                        Just v -> v
                        Nothing -> valor
            in
            Just (max valor (max maxIzq maxDer))

-- 14. Buscar Por Predicado


buscarPor : (a -> Bool) -> Tree a -> Maybe a
buscarPor predicado arbol =
    case arbol of
        Empty ->
            Nothing

        Node valor izq der ->
            if predicado valor then
                Just valor
            else
                case buscarPor predicado izq of
                    Just x ->
                        Just x
                    Nothing ->
                        buscarPor predicado der


-- 15. Obtener Valor de Raíz


raiz : Tree a -> Maybe a
raiz arbol =
    case arbol of
        Empty ->
            Nothing

        Node valor _ _ ->
            Just valor


-- 16. Obtener Hijo Izquierdo

hijoIzquierdo : Tree a -> Maybe (Tree a)
hijoIzquierdo arbol =
    case arbol of
        Empty ->
            Nothing

        Node _ izq _ ->
            Just izq

hijoDerecho : Tree a -> Maybe (Tree a)
hijoDerecho arbol =
    case arbol of
        Empty ->
            Nothing

        Node _ _ der ->
            Just der

-- 17. Obtener Nieto


nietoIzquierdoIzquierdo : Tree a -> Maybe (Tree a)
nietoIzquierdoIzquierdo arbol =
    hijoIzquierdo arbol
        |> Maybe.andThen hijoIzquierdo


-- 18. Buscar en Profundidad

obtenerSubarbol : a -> Tree a -> Maybe (Tree a)
obtenerSubarbol valorBuscar arbol =
    case arbol of
        Empty ->
            Nothing

        Node valor izq der ->
            if valor == valorBuscar then
                Just arbol
            else
                case obtenerSubarbol valorBuscar izq of
                    Just sub ->
                        Just sub
                    Nothing ->
                        obtenerSubarbol valorBuscar der


buscarEnSubarbol : a -> a -> Tree a -> Maybe a
buscarEnSubarbol valor1 valor2 arbol =
    obtenerSubarbol valor1 arbol
        |> Maybe.andThen (buscar valor2)


-- ============================================================================
-- PARTE 3: RESULT PARA VALIDACIONES
-- ============================================================================


-- 19. Validar No Vacío


validarNoVacio : Tree a -> Result String (Tree a)
validarNoVacio arbol =
    case arbol of
        Empty ->
            Err "El árbol está vacío"

        _ ->
            Ok arbol


-- 20. Obtener Raíz con Error


obtenerRaiz : Tree a -> Result String a
obtenerRaiz arbol =
    case arbol of
        Empty ->
            Err "No se puede obtener la raíz de un árbol vacío"

        Node valor _ _ ->
            Ok valor


-- 21. Dividir en Valor Raíz y Subárboles


dividir : Tree a -> Result String ( a, Tree a, Tree a )
dividir arbol =
    case arbol of
        Empty ->
            Err "No se puede dividir un árbol vacío"

        Node valor izq der ->
            Ok (valor, izq, der)


-- 22. Obtener Mínimo con Error


obtenerMinimo : Tree comparable -> Result String comparable
obtenerMinimo arbol =
    case encontrarMinimo arbol of
        Just minVal ->
            Ok minVal

        Nothing ->
            Err "No hay mínimo en un árbol vacío"
-- aca reutilizo la funcion encontrarMinimo que ya devuelve Maybe


-- 23. Verificar si es BST


esBST : Tree comparable -> Bool
esBST arbol =
    case arbol of
        Empty ->
            True

        Node valor izq der ->
            todosMenoresQue valor izq
                && todosMayoresQue valor der
                && esBST izq
                && esBST der


todosMenoresQue : comparable -> Tree comparable -> Bool
todosMenoresQue limite arbol =
    case arbol of
        Empty ->
            True

        Node valor izq der ->
            valor < limite
                && todosMenoresQue limite izq
                && todosMenoresQue limite der


todosMayoresQue : comparable -> Tree comparable -> Bool
todosMayoresQue limite arbol =
    case arbol of
        Empty ->
            True

        Node valor izq der ->
            valor > limite
                && todosMayoresQue limite izq
                && todosMayoresQue limite der


-- 24. Insertar en BST


insertarBST : comparable -> Tree comparable -> Result String (Tree comparable)
insertarBST valor arbol =
    Err "El valor ya existe en el árbol"


-- 25. Buscar en BST


buscarEnBST : comparable -> Tree comparable -> Result String comparable
buscarEnBST valor arbol =
    Err "El valor no se encuentra en el árbol"


-- 26. Validar BST con Result


validarBST : Tree comparable -> Result String (Tree comparable)
validarBST arbol =
    Err "El árbol no es un BST válido"


-- ============================================================================
-- PARTE 4: COMBINANDO MAYBE Y RESULT
-- ============================================================================


-- 27. Maybe a Result


maybeAResult : String -> Maybe a -> Result String a
maybeAResult mensajeError maybe =
    case maybe of
        Just valor ->
            Ok valor

        Nothing ->
            Err mensajeError


-- 28. Result a Maybe


resultAMaybe : Result error value -> Maybe value
resultAMaybe resultado =
    case resultado of
        Ok valor ->
            Just valor

        Err _ ->
            Nothing


-- 29. Buscar y Validar


buscarPositivo : Int -> Tree Int -> Result String Int
buscarPositivo valor arbol =
    Err "El valor no se encuentra en el árbol"


-- 30. Pipeline de Validaciones


validarArbol : Tree Int -> Result String (Tree Int)
validarArbol arbol =
    Err "Validación fallida"


-- 31. Encadenar Búsquedas


buscarEnDosArboles : Int -> Tree Int -> Tree Int -> Result String Int
buscarEnDosArboles valor arbol1 arbol2 =
    Err "Búsqueda fallida"


-- ============================================================================
-- PARTE 5: DESAFÍOS AVANZADOS
-- ============================================================================


-- 32. Recorrido Inorder


inorder : Tree a -> List a
inorder arbol =
    case arbol of
        Empty ->
            []

        Node valor izq der ->
            inorder izq ++ (valor :: inorder der)

-- 33. Recorrido Preorder


preorder : Tree a -> List a
preorder arbol =
    case arbol of
        Empty ->
            []

        Node valor izq der ->
            valor :: (preorder izq ++ preorder der)

-- 34. Recorrido Postorder


postorder : Tree a -> List a
postorder arbol =
    case arbol of
        Empty ->
            []

        Node valor izq der ->
            postorder izq ++ postorder der ++ [valor]


-- 35. Map sobre Árbol


mapArbol : (a -> b) -> Tree a -> Tree b
mapArbol funcion arbol =
    case arbol of
        Empty ->
            Empty

        Node valor izq der ->
            Node (funcion valor) (mapArbol funcion izq) (mapArbol funcion der)


-- 36. Filter sobre Árbol


filterArbol : (a -> Bool) -> Tree a -> Tree a
filterArbol predicado arbol =
    case arbol of
        Empty ->
            Empty

        Node valor izq der ->
            if predicado valor then
                Node valor (filterArbol predicado izq) (filterArbol predicado der)
            else
                -- Si el nodo no cumple lo eliminamos y combinamos los subárboles
                combinarArboles (filterArbol predicado izq) (filterArbol predicado der)


-- Función auxiliar para combinar dos árboles cuando eliminamos la raíz
combinarArboles : Tree a -> Tree a -> Tree a
combinarArboles arbol1 arbol2 =
    case arbol1 of
        Empty ->
            arbol2

        Node val izq der ->
            Node val izq (combinarArboles der arbol2)

-- 37. Fold sobre Árbol


foldArbol : (a -> b -> b) -> b -> Tree a -> b
foldArbol funcion acumulador arbol =
    case arbol of
        Empty ->
            acumulador

        Node valor izq der ->
            let
                acumDer = foldArbol funcion acumulador der
                acumIzq = foldArbol funcion acumDer izq
            in
            funcion valor acumIzq


-- 38. Eliminar de BST


eliminarBST : comparable -> Tree comparable -> Result String (Tree comparable)
eliminarBST valor arbol =
    Err "El valor no existe en el árbol"


-- 39. Construir BST desde Lista


desdeListaBST : List comparable -> Result String (Tree comparable)
desdeListaBST lista =
    Err "Valor duplicado"


-- 40. Verificar Balance


estaBalanceado : Tree a -> Bool
estaBalanceado arbol =
    case arbol of
        Empty ->
            True

        Node _ izq der ->
            abs (altura izq - altura der) <= 1
                && estaBalanceado izq
                && estaBalanceado der


-- 41. Balancear BST


balancear : Tree comparable -> Tree comparable
balancear arbol =
    Empty


-- 42. Camino a un Valor


type Direccion
    = Izquierda
    | Derecha


encontrarCamino : a -> Tree a -> Result String (List Direccion)
encontrarCamino valor arbol =
    case arbol of
        Empty ->
            Err "El valor no existe en el árbol"

        Node valorRaiz izq der ->
            if valor == valorRaiz then
                Ok []

            else
                case encontrarCamino valor izq of
                    Ok camino ->
                        Ok (Izquierda :: camino)

                    Err _ ->
                        case encontrarCamino valor der of
                            Ok camino ->
                                Ok (Derecha :: camino)

                            Err _ ->
                                Err "El valor no existe en el árbol"


-- 43. Seguir Camino


seguirCamino : List Direccion -> Tree a -> Result String a
seguirCamino camino arbol =
    case (camino, arbol) of
        ([], Node valor _ _) ->
            Ok valor

        (Izquierda :: resto, Node _ izq _) ->
            if esVacio izq then
                Err "Camino inválido"
            else
                seguirCamino resto izq

        (Derecha :: resto, Node _ _ der) ->
            if esVacio der then
                Err "Camino inválido"
            else
                seguirCamino resto der

        (_ :: _, Empty) ->
            Err "Camino inválido"

        ([], Empty) ->
            Err "Camino inválido"


-- 44. Ancestro Común Más Cercano


ancestroComun : comparable -> comparable -> Tree comparable -> Result String comparable
ancestroComun valor1 valor2 arbol =
    Err "Uno o ambos valores no existen en el árbol"


-- ============================================================================
-- PARTE 6: DESAFÍO FINAL - SISTEMA COMPLETO
-- ============================================================================


-- 45. Sistema Completo de BST
-- (Las funciones individuales ya están definidas arriba)


-- Operaciones que retornan Bool
esBSTValido : Tree comparable -> Bool
esBSTValido arbol =
    esBST arbol


estaBalanceadoCompleto : Tree comparable -> Bool
estaBalanceadoCompleto arbol =
    estaBalanceado arbol


contieneValor : comparable -> Tree comparable -> Bool
contieneValor valor arbol =
    contiene valor arbol


-- Operaciones que retornan Maybe
buscarMaybe : comparable -> Tree comparable -> Maybe comparable
buscarMaybe valor arbol =
    buscar valor arbol


encontrarMinimoMaybe : Tree comparable -> Maybe comparable
encontrarMinimoMaybe arbol =
    encontrarMinimo arbol


encontrarMaximoMaybe : Tree comparable -> Maybe comparable
encontrarMaximoMaybe arbol =
    encontrarMaximo arbol


-- Operaciones que retornan Result
insertarResult : comparable -> Tree comparable -> Result String (Tree comparable)
insertarResult valor arbol =
    insertarBST valor arbol


eliminarResult : comparable -> Tree comparable -> Result String (Tree comparable)
eliminarResult valor arbol =
    eliminarBST valor arbol


validarResult : Tree comparable -> Result String (Tree comparable)
validarResult arbol =
    validarBST arbol


obtenerEnPosicion : Int -> Tree comparable -> Result String comparable
obtenerEnPosicion posicion arbol =
    Err "Posición inválida"


-- Operaciones de transformación
map : (a -> b) -> Tree a -> Tree b
map funcion arbol =
    mapArbol funcion arbol


filter : (a -> Bool) -> Tree a -> Tree a
filter predicado arbol =
    filterArbol predicado arbol


fold : (a -> b -> b) -> b -> Tree a -> b
fold funcion acumulador arbol =
    foldArbol funcion acumulador arbol


-- Conversiones
aLista : Tree a -> List a
aLista arbol =
    inorder arbol


desdeListaBalanceada : List comparable -> Tree comparable
desdeListaBalanceada lista =
    Empty
