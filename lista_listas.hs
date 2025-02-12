import Data.Char (isDigit, toUpper, isAlpha, isLetter)
import GHC.Base (VecElem(Int16ElemRep))
import System.Win32 (xBUTTON1)
import Language.Haskell.TH (Lit(IntegerL))


-- SLIDE

ehPar :: Integer -> Bool
ehPar x = mod x 2 == 0

dobraListaPar :: [Integer] -> [Integer]
dobraListaPar xs = [2 * a | a <- xs, ehPar a, a > 3]

digits :: String -> String
digits st = [ch | ch <- st, isDigit ch]

todosPares :: [Integer] -> Bool
todosPares xs = xs == [x | x <- xs , ehPar x]

numPares :: [Integer] -> [Integer]
numPares xs = [x | x <- xs, ehPar x]

firstDigit st = case(digits st) of
    [] -> '\0'
    (a:as) -> a

maiorLista [] = minBound :: Int
maiorLista [x] = x
maiorLista (x:xs)
    | x > maiorLista xs = x
    | otherwise = maiorLista xs

qsort [] = []
qsort (x:xs) = menorIgualX ++ [x] ++ maiorX
    where
        menorIgualX = qsort [y | y <- xs, y <= x]
        maiorX = qsort [y | y <- xs, y > x]

-- QUESTÃO 1

paraMaiuscula :: String -> String
paraMaiuscula xs = [toUpper x | x <- xs]

paraMaiuscula2 :: String -> String
paraMaiuscula2 xs = [toUpper x | x <- xs, isLetter x]

-- QUESTÃO 2

divisores :: Integer -> [Integer]
divisores n
    | n <= 0 = []
    | otherwise = [x | x <- [1 .. n], mod n x == 0]

-- QUESTÃO 3

menorLista :: [Int] -> Int
menorLista [] = maxBound :: Int
menorLista [x] = x
menorLista (x:xs)
    | x < menorLista xs = x
    | otherwise = menorLista xs

-- QUESTÃO 4

measure :: [Int] -> Int
measure [] = -1
measure x = length x

-- QUESTÃO 5

takeFinal :: [a] -> Int -> [a]
takeFinal l 0 = []
takeFinal l n = l!!(length l - n):[] ++ takeFinal l (n - 1)

takeFinal2 :: [a] -> Int -> [a]
takeFinal2 xs n = drop (length xs - n) xs

takeFinal3 :: [a] -> Int -> [a]
takeFinal3 xs n = reverse (take n (reverse xs))

-- QUESTÃO 6

remove :: Int -> [a] -> [a]
remove n xs = take n xs ++ drop (n+1) xs

remove2 :: Int -> [a] -> [a]
remove2 _ [] = []
remove2 n (x:xs)
    | n == 0 = xs
    | otherwise = x : remove2 (n - 1) xs

-- QUESTÃO 7

firstInt :: [Int] -> Int
firstInt (x:xs) = x + 1
firstInt [] = 0

firstInt2 :: [Int] -> Int
firstInt2 x = if null x then 0 else head x + 1

firstInt3 :: [Int] -> Int
firstInt3 xs = case xs of
    (x:_) -> x +1
    [] -> 0

-- QUESTÃO 8

sumFirstTwo :: [Int] -> Int
sumFirstTwo [] = 0
sumFirstTwo [x] = x
sumFirstTwo (x:y:xs) = x + y

sumFirstTwo2 :: [Int] -> Int
sumFirstTwo2 xs
    | null xs = 0
    | length xs == 1 = head xs
    | otherwise = head xs + head (tail xs)

-- QUESTÃO 9

produto :: [Integer] -> Integer
produto [] = 1
produto (x:xs) = x * produto xs

produto2 :: [Integer] -> Integer
produto2 xs = foldr (*) 1 xs


-- QUESTÃO 10

unique :: [Integer] -> [Integer]
unique xs = [x | x <- xs, count x xs == 1]

count :: Eq a => a -> [a] -> Int
count x xs = length [y | y <- xs, y == x]

-- QUESTÃO 11

inOrder :: Ord a => [a] -> Bool
inOrder [] = True
inOrder [x] = True
inOrder (x:y:xs)
    | x > y = False
    | otherwise = inOrder xs