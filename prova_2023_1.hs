import GHC.Generics (prec)
-- QUESTAO 1

type Equipamento = String
type Uso = (Equipamento, Int)
type ListaUso = [Uso]

inv :: ListaUso -> Bool
inv [] = True
inv (x:xs) = snd x >= 0 && inv xs

inv2 :: ListaUso -> Bool
inv2 = foldr (\ x -> (&&) (snd x >= 0)) True

duracaoDe :: Equipamento -> ListaUso -> Int
duracaoDe e [] = 0
duracaoDe e (x:xs)
    | fst x == e = snd x
    | otherwise = duracaoDe e xs

bemFormada :: ListaUso -> Bool
bemFormada l = inv l && length [x | x <- l, snd x < 24] == length l

removerEqp :: Equipamento -> ListaUso -> ListaUso
removerEqp e l = [x | x <- l, fst x /= e]

type Preco = Int
type Tarifa = (Equipamento, Preco)
type Tarifas = [Tarifa]

definidoEm :: ListaUso -> Tarifas -> Bool
definidoEm l t = all (\e -> fst e `elem` (map fst t)) l

preco :: Equipamento -> Tarifas -> Int
preco _ [] = error "Tarifa Vazia"
preco e (x:xs)
    | fst x == e = snd x
    | otherwise = preco e xs

precoDe :: ListaUso -> Tarifas -> Preco
precoDe [] _ = 0
precoDe (x:xs) t = let p = preco (fst x) t in (p * snd x) + precoDe xs t

-- QUESTÃO 2

zeroVendas1 :: Int -> Int
zeroVendas1 n = length [x | x <- [0..n], vendas x == 0]

zeroVendas2 :: Int -> Int
zeroVendas2 n = length (filter (\s -> vendas s == 0) [0..n])

main :: IO()
main = do 
    let e1 = ("maquina_de_lavar", 2)
    let e2 = ("cafeteira", 1)
    let e3 = ("lava_loucas", 2)
    let l = [e1,e2,e3,e1,e2]

    let tr1 = ("maquina_de_lavar", 20)
    let tr2 = ("cafeteira", 3)
    let tr3 = ("lava_loucas", 15)
    let trf = [tr1,tr2,tr3]

    print(precoDe l trf)
