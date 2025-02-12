import System.Win32 (xBUTTON1)
-- QUESTÃO 1

f :: [Int] -> [Int]
f [] = []
f [x] = []
f (x:xs)
    | x == head xs = x : f xs
    | otherwise = f xs

f2 :: [Int] -> [Int]
f2 xs = [x | (x,y) <- zip xs (tail xs), x == y]

-- QUESTÃO 2

-- a)
testaElementos :: (a -> Bool) -> [a] -> Bool
testaElementos f [] = True
testaElementos f (x:xs) = f x && testaElementos f xs

-- b)
testaElementos2 :: (a -> Bool) -> [a] -> Bool
testaElementos2 f xs = and (map f xs)

-- c)
testaElementos3 :: (a -> Bool) -> [a] -> Bool
testaElementos3 f = foldr ((&&) . f) True

testaElementos3' :: (a -> Bool) -> [a] -> Bool
testaElementos3' f= foldr (\x acc -> f x && acc) True

-- QUESTÃO 3

sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas (x:xs) = map (x:) (sublistas xs) ++ sublistas xs

-- QUESTÃO 4

-- a)

poli :: Integer -> Integer -> Integer -> (Integer -> Integer)
poli a b c = (\x -> a * x^2 + b * x + c)

-- b)

listaPoli :: [(Integer, Integer, Integer)] -> [Integer -> Integer]
listaPoli l = [poli a b c | (a,b,c) <- l]

-- c)
 
appListaPoli :: [Integer -> Integer] -> [Integer] -> [Integer]
appListaPoli [] [] = []
appListaPoli (a:as) (b:bs) = a b : appListaPoli as bs

-- QUESTÃO 5

-- a)

newtype Pilha a = Pilha [a] deriving (Show)

-- b)

top :: Pilha a -> a
top (Pilha []) = error "Pilha Vazia!"
top (Pilha (x:_)) = x

push :: Pilha a -> a -> Pilha a
push (Pilha []) a = Pilha [a]
push (Pilha xs) a = Pilha (a : xs)

pop :: Pilha a -> Pilha a
pop (Pilha []) = error "Pilha Vazia!"
pop (Pilha (_:xs)) = Pilha xs

