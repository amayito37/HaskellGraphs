--Eduardo Amaya Espinosa, 78644798R

data Vertice = A|B|C|D|E|F deriving (Eq, Read, Show)
data Grafo = G [Vertice] [(Vertice,Vertice)] deriving (Read, Show)

instance Eq Grafo where
    (G xs ys) == (G zs ts) = isomorfos (G xs ys) (G zs ts)

g1 :: Grafo
g1 = G [B, D, E, C] [(D,E),(E,B),(C,B),(E,C)]
g2 :: Grafo
g2 = G [D, F, E] [(D,F),(E,D),(D,E),(F,E)]
g3 :: Grafo
g3 = G [A, C, D] [(A,C),(C,D),(A,D)]
g4 :: Grafo
g4 = G [A, F, D, E] [(E,A),(F,D),(D,E),(D,A)] --isomorfo a g1
g5 :: Grafo
g5 = G [A, F, D, E] [(D,F),(F,D),(F,A),(E,A)] --no conexo
g6 :: Grafo
g6 = G [A,B,C,D,E,F] [(A,C),(D,F),(E,F),(D,A),(C,B),(B,E),(D,B)] --conexo
g7 :: Grafo
g7 = G [B,C,A,F,E,D] [(B,F),(C,D),(A,D),(C,B),(F,E),(E,A),(C,E)] --isomorfo a g6
g8 :: Grafo
g8 = G [B,C,A,F,E,D] [(B,F),(C,D),(A,D),(C,B),(F,E),(E,A),(C,E),(C,D)] --como g7 pero con una arista doble


es_grafo :: Grafo -> Bool
es_grafo (G [] _) = False
es_grafo (G xs ys) = sinRepetidos xs && perteneceAristas ys xs 

--Función que devuelve False si una lista tiene repetidos y True si no
sinRepetidos :: Eq a => [a] -> Bool
sinRepetidos [] = True
sinRepetidos (x:xs) = not (elem x xs) && sinRepetidos xs

--Función que devuelve True si todas las aristas conectan vértices válidos
perteneceAristas :: Eq a => [(a,a)] -> [a] -> Bool
perteneceAristas [] _ = True
perteneceAristas xs ys = and $ map (\(x,y) -> elem x ys && elem y ys) xs

mat_ady :: Grafo -> [[Int]]
mat_ady (G [] _) = []
mat_ady (G xs ys) = let matriz = [initLista (length xs) | i <- [1..(length xs)]] in act_mat_ady matriz xs ys

--Función que dada una matriz de adyacencia y una lista de aristas añade las aristas a la matriz
act_mat_ady :: [[Int]] -> [Vertice] -> [(Vertice, Vertice)] -> [[Int]]
act_mat_ady mat _ [] = mat
act_mat_ady mat xs ((a,b):ys) = let {mat1 = act_mat_ady mat xs ys; x = vertANum a xs; y = vertANum b xs; lista = mat1 !! x; div = splitAt x mat1} in (fst div) ++ ((sumarAElemento lista y):(tail $ snd div))   

--Función que dado un vértice devuelve el número que ocupa en la lista de vértices del grafo
vertANum :: Vertice -> [Vertice] -> Int
vertANum _ [] = (-1)
vertANum v (x:xs)
                | v == x = 0
                | otherwise = 1 + vertANum v xs

--Función que dada una lista y una posición suma uno al elemento de esa posición
sumarAElemento :: Num a => [a] -> Int -> [a]
sumarAElemento [] _ = []
sumarAElemento xs n = let {elem = (xs !! n) + 1; div = splitAt n xs} in (fst div) ++ (elem:(tail $ snd div))

--Función que inicializa una lista a 0 con el tamaño deseado
initLista :: Int -> [Int]
initLista n = [0 | i <- [1..n]]


grados_pos :: Grafo -> [Int]
grados_pos (G [] _) = []
grados_pos (G (x:xs) ys) = (numAristas x ys):(grados_pos (G xs ys)) 
                        where numAristas x [] = 0
                              numAristas x ((_,b):ys)
                                                    | x == b = 1 + numAristas x ys
                                                    | otherwise = numAristas x ys
                                                    
grados_neg :: Grafo -> [Int]
grados_neg (G [] _) = []
grados_neg (G (x:xs) ys) = (numAristas x ys):(grados_neg (G xs ys)) 
                        where numAristas x [] = 0
                              numAristas x ((a,_):ys)
                                                    | x == a = 1 + numAristas x ys
                                                    | otherwise = numAristas x ys

--Devuelve todos los caminos de longitud n partiendo de v salvo los que tienen repetidos
--(Los ciclos que solo repiten el primer y último elemento sí se cuentan)
camino_lng :: Grafo -> Vertice -> Int -> [[Vertice]]
camino_lng (G [] _) v _ = [[v]]
camino_lng (G _ _) v 0 = [[v]]
camino_lng (G xs ys) v n = filter (not . null) $ concat $ map (\x -> (if elem x xs then concatenar x v else [])) (adyacentes (G xs ys) v)
                           where concatenar x v = map (\zs -> if (elem v zs && zs !! ((length zs) - 1) /= v) then [] else v:zs) (camino_lng (G (borrar x xs) ys) x (n-1))


--Devuelve todos los vértices adyacentes a uno dado
adyacentes :: Grafo -> Vertice -> [Vertice]
adyacentes (G _ []) _ = []
adyacentes (G xs ((a,b):ys)) v = if (a == v && elem b xs) then b:(adyacentes (G xs ys) v) else adyacentes (G xs ys) v

--Función que dado un elemento y una lista te borra la primera aparición del elemento en la lista
borrar :: Eq a => a -> [a] -> [a]
borrar _ [] = []
borrar y (x:xs) = if x == y then xs else x:(borrar y xs)


conexo :: Grafo -> Bool
conexo (G [] _) = True
conexo (G xs ys) = or [and [existeCamino (G xs ys) v w | w <- xs] | v <- xs]

--Dados dos vértices decide si hay un camino del primero al segundo
existeCamino :: Grafo -> Vertice -> Vertice -> Bool
existeCamino (G _ []) v w = False
existeCamino (G xs ys) v w
                            | v == w = True
                            | otherwise = let adyacencia = adyacentes (G xs ys) v in elem w adyacencia || (or $ map (\x -> existeCamino (G (borrar v xs) ys) x w) adyacencia)


--Devuelve true si los dos grafos son isomorfos, false en caso contrario.--Devuelve true si los dos grafos son isomorfos, false en caso contrario.
isomorfos :: Grafo -> Grafo -> Bool
isomorfos (G [] _) (G [] _) = True
isomorfos (G [] _) (G _ _) = False
isomorfos (G _ _) (G [] _) = False
isomorfos (G xs ys) (G zs ts) = (length xs == length zs) && (length ys == length ts) --mismo número de vértices y aristas
                                && iguales (grados_pos (G xs ys)) (grados_pos (G zs ts)) --mismo número de vértices con el mismo grado
                                && iguales (grados_neg (G xs ys)) (grados_neg (G zs ts))
                                && isomorfismo (G xs ys) (zip3 xs (grados_pos (G xs ys)) --al menos un isomorfismo entre ellos
                                (grados_neg (G xs ys))) (G zs ts) (zip3 zs (grados_pos (G zs ts)) (grados_neg (G zs ts)))

--Devuelve true si existe un isomorfismo entre los dos grafos (sabiendo de antemano que los dos 
--grafos tienen mismo número de vértices y aristas y mismos grados) y false en caso contrario
isomorfismo :: Grafo -> [(Vertice,Int,Int)] -> Grafo -> [(Vertice,Int,Int)] -> Bool
isomorfismo (G xs ys) g1s (G zs ts) g2s = or $ map (esIsomorfismo ys ts) (biyecciones g1s g2s)

--Genera todas las biyecciones posibles entre dos grafos (que respeten los grados)
biyecciones :: [(Vertice,Int,Int)] -> [(Vertice,Int,Int)] -> [[(Vertice, Vertice)]]
biyecciones [] _ = [[]]
biyecciones _ [] = [[]]
biyecciones (g1:g1s) g2s = concat $ map (\(a,b) -> concatenar (a,b) (g1:g1s) g2s) (emparejar g1 g2s)
                           where emparejar g1 g2s = map (\x -> (vertice g1,x)) (mismosGrados g1 g2s)
                                 concatenar (a,b) (g1:g1s) g2s = map (\xs -> (a,b):xs) (biyecciones g1s (borrarTupla b g2s))
                                 vertice (a,_,_) = a

--Dada una biyección decide si es un isomorfismo
esIsomorfismo :: [(Vertice, Vertice)] -> [(Vertice, Vertice)] -> [(Vertice, Vertice)] -> Bool
esIsomorfismo ars1 ars2 biys = and $ map (\(a,b) -> (elem (par a biys, par b biys) ars2)) ars1


--Dado un elemento y una biyección encuentra su par
par :: Vertice -> [(Vertice, Vertice)] -> Vertice
par x ((a,b):xs)
                |x == a = b
                |otherwise = par x xs

--Devuelve true si dos listas tienen los mismos elementos, sin importar el orden
iguales :: Eq a => [a] -> [a] -> Bool
iguales [] _ = True
iguales (x:xs) y = elem x y && iguales xs y

--Borra el vértice deseado de la lista de vértices y grados
borrarTupla :: Vertice -> [(Vertice,Int,Int)] -> [(Vertice,Int,Int)]
borrarTupla _ [] = []
borrarTupla a ((x,y,z):xs)
                        | a == x = xs
                        | otherwise = (x,y,z):(borrarTupla a xs)

--Dado un vértice y sus grados devuelve todos los vértices de una lista 
--que tengan el mismo grado negativo y positivo que él
mismosGrados :: (Vertice,Int,Int) -> [(Vertice,Int,Int)] -> [Vertice]
mismosGrados _ [] = []
mismosGrados (a,b,c) ((x,y,z):xs)
                              |(b == y) && (c == z) = x:(mismosGrados (a,b,c) xs)
                              |otherwise = mismosGrados (a,b,c) xs


leegrafo :: IO Grafo
leegrafo = do vertices <- (leeVertices [])
              aristas <- (leeAristas [])
              if es_grafo (G vertices aristas) then do return (G vertices aristas)
              else do putStrLn "Los datos introducidos no son correctos"
                      leegrafo


--Lee los vértices y los devuelve en una lista
leeVertices :: [Vertice] -> IO [Vertice]
leeVertices xs = do putStr "Introduce un vértice (0 para terminar): "
                    line <- getLine
                    if ('0' == (head line)) then do return xs
                    else do resto <- (leeVertices xs)
                            return ((read line::Vertice):resto)

--Lee las aristas y las devuelve en una lista
leeAristas :: [(Vertice,Vertice)] -> IO [(Vertice,Vertice)]
leeAristas xs = do putStr "Introduce una arista con el formato (vértice,vértice), (0 para terminar): "
                   line <- getLine
                   if ('0' == (head line)) then do return xs
                   else do resto <- (leeAristas xs)
                           return ((read line::(Vertice,Vertice)):resto)


muestra_matriz :: IO ()
muestra_matriz = do grafo <- leegrafo
                    imprimirMatriz $ listaFilas (mat_ady grafo)
                    return ()



--Dada una matriz la convierte a una lista de cadenas
listaFilas :: Show a => [[a]] -> [String]
listaFilas = map imprimirFila

--Dada una fila de una matriz la devuelve en forma de cadena
--con los elementos separados por espacios
imprimirFila :: Show a => [a] -> String
imprimirFila (x:[]) = show x
imprimirFila (x:xs) = foldl (\x y -> x ++ [' '] ++ show y) (show x) xs

--Dada una lista de cadenas, imprime una en cada linea
imprimirMatriz :: [String] -> IO ()
imprimirMatriz [] = return ()
imprimirMatriz (x:xs) = if not (null x) then do putStrLn x
                                                imprimirMatriz xs
                        else do imprimirMatriz xs


muestra_caminos :: IO ()
muestra_caminos = do (G xs ys) <- leegrafo
                     if conexo (G xs ys) then do imprimirMatriz $ map (\vertice -> imprimirCamino (camino (G xs ys) (elegirVertice (G xs ys)) vertice)) xs 
                                                 return ()
                     else do putStrLn "El grafo introducido no es conexo"
                             return ()


elegirVertice :: Grafo -> Vertice
elegirVertice (G (x:xs) ys) = if and [existeCamino (G (x:xs) ys) x w | w <- xs] then x else elegirVertice (G (xs ++ [x]) ys)  

--Dados dos vértices decide si hay un camino del primero al segundo
camino :: Grafo -> Vertice -> Vertice -> [Vertice]
camino (G _ []) v w = []
camino (G xs ys) v w
                    | v == w = []
                    | otherwise = let adyacencia = adyacentes (G xs ys) v in if elem w adyacencia then [v,w] else (let lista = [cam | cam <- (map (\t -> v:(camino (G (borrar v xs) ys) t w)) adyacencia), (cam !! ((length cam) - 1)) == w] in if null lista then [] else head lista) 

imprimirCamino :: Show a => [a] -> String
imprimirCamino [] = []
imprimirCamino (x:xs) = foldl (\x y -> x ++ ['-'] ++ show y) (show x) xs

