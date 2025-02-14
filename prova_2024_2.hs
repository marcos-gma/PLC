-- QUESTÃO 1

testaElementos1 :: (a -> Bool) -> [a] -> Bool -- Versão Recursiva
testaElementos1 f [] = True
testaElementos1 f (x:xs) = f x && testaElementos1 f xs

testaElementos2 :: (a -> Bool) -> [a] -> Bool -- Versão com Map, And e .
testaElementos2 f = and . map f 

testaElementos3 :: (a -> Bool) -> [a] -> Bool -- Versão com foldr, fiz sem usar abstração lambda
testaElementos3 f = foldr ((&&) . f) True

-- QUESTÃO 2

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

intDiv:: Int -> Int -> Int
intDiv a b 
    | a < b = 0
    | otherwise = 1 + intDiv (a-b) b

metade :: [a] -> ([a],[a])
metade xs = (take (intDiv (length xs) 2) xs, drop (intDiv (length xs) 2) xs)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort(fst (metade xs))) (msort(snd (metade xs)))


-- QUESTÃO 3 


data BTree = Leaf | Node (BTree) Int (BTree) deriving (Show) 

inserirValor :: Int -> BTree -> BTree
inserirValor n Leaf = Node Leaf n Leaf
inserirValor n (Node l m r)
    | n > m = Node l m (inserirValor n r)
    | otherwise = Node (inserirValor n l) m r

arvLista :: BTree -> [Int]
arvLista Leaf = []
arvLista (Node l m r) = arvLista l ++ [m] ++ arvLista r

somaArv :: BTree -> Int
somaArv Leaf = 0
somaArv (Node l m r) = somaArv l + m + somaArv r

listaArv :: [Int] -> BTree
listaArv [] = Leaf
listaArv (x:xs) = inserirValor x (listaArv xs)