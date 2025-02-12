import GHC.Exts.Heap (GenClosure(key, n_args))
-- QUESTÃO 1:

dobro :: Integer -> Integer
dobro x = x * 2

-- QUESTÃO 2:

quadruplo :: Integer -> Integer
quadruplo x = dobro (dobro x)

-- QUESTÃO 3:

poli2 :: Double -> Double -> Double -> Double -> Double
poli2 a b c x = a * (x*x) + b * x + c

-- QUESTÃO 4:

parImpar :: Integer -> String
parImpar n
    | mod n 2 == 0 = "par"
    | otherwise = "impar"

parImpar2 :: Integer -> String
parImpar2 n = if mod n 2 == 0 then "par" else "ímpar"


-- QUESTÃO 5:

maxThree :: Integer -> Integer -> Integer -> Integer 
maxThree x y z 
    | x >= y && x >= z = x
    | y >= z = y
    | otherwise = z


maxFour :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour x y z w 
    | w >= maxThree x y z = w
    | otherwise = maxThree x y z

maxFour2 :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour2 x y z w = max (max x y) (max z w)

maxFour3 :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour3 x y z w = max (maxThree x y z) w

-- QUESTÃO 6:

quantosIguais :: Integer -> Integer -> Integer -> Integer
quantosIguais x y z
    | x == y && y == z = 3
    | x == y || x == z || y == z = 2
    | otherwise = 0

-- QUESTÃO 7:

ehZero :: Integer -> Bool
ehZero x
    | x == 0 = True
    | otherwise = False

-- QUESTÃO 8:

sumTo :: Integer -> Integer
sumTo n
    | n == 0 = 0
    | otherwise = n + sumTo (n - 1)

-- QUESTÃO 9:

potencia :: Integer -> Integer -> Integer
potencia n k
    | k == 0 = 1
    | k == 1 = n
    | otherwise = n * potencia n (k-1)

-- QUESTÃO 10

coefBin :: Integer -> Integer -> Integer
coefBin _ 0 = 1
coefBin 0 _ = 1
coefBin n k = coefBin (n-1) k + coefBin (n-1) (k-1)

-- QUESTÃO 11:

tribonacci :: Integer -> Integer
tribonacci n
    | n == 1 = 1
    | n == 2 = 1
    | n == 3 = 2
    | otherwise = tribonacci (n-1) + tribonacci (n-2) + tribonacci (n-3)

tribonacci2 :: Integer -> Integer
tribonacci2 n = tribAux n 1 1 2
  where
    tribAux 1 a _ _ = a
    tribAux 2 _ b _ = b
    tribAux 3 _ _ c = c
    tribAux n a b c = tribAux (n - 1) b c (a + b + c)

-- QUESTÃO 12:

addEspacos :: Int -> String
addEspacos n
    | n == 0 = ""
    | otherwise = " " ++ addEspacos (n-1) 

-- QUESTÃO 13:

paraDireita :: Int -> String -> String
paraDireita n s = addEspacos n ++ s

-- QUESTÃO 14:

vendas :: Integer -> Integer
vendas 0 = 12
vendas 1 = 14
vendas 2 = 15

cabecalho :: String
cabecalho = "Semana  Venda\n"

imprimeSemanas :: Integer -> String
imprimeSemanas 0 = addEspacos 2 ++ "0" ++ paraDireita 6 (show (vendas 0)) ++ "\n"
imprimeSemanas n = imprimeSemanas (n-1) ++ addEspacos 2 ++ show n ++ paraDireita 6 (show (vendas n)) ++ "\n"

total :: Integer -> Integer
total 0 = vendas 0
total n = vendas n + total (n - 1)

imprimeTotal :: Integer -> String
imprimeTotal n = "Total" ++ addEspacos 4 ++ show (total n) ++ "\n"

media :: Integer -> Double
media n = (fromInteger (total n) * 1.0) / (fromInteger (n + 1) * 1.0)

imprimeMedia :: Integer -> String
imprimeMedia n = "Media" ++ addEspacos 4 ++ show (media n) ++ "\n"

imprimeTabela :: Integer -> String
imprimeTabela n = cabecalho
                    ++ imprimeSemanas n
                    ++ imprimeTotal n
                    ++ imprimeMedia n