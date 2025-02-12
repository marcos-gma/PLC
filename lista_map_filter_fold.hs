import Data.Char (toUpper, isLetter, isAlpha)
import Language.Haskell.TH (Strict)
import System.Win32 (xBUTTON1)
-- QUESTÃO 1

to1 :: Num a => p -> a
to1 x = 1

myLength :: [a] -> Int
myLength xs = sum (map to1 xs)

-- QUESTÃO 2

-- a)

uppers :: String -> String
uppers = map toUpper

-- b)

doubles :: [Int] -> [Int]
doubles = map (*2)

-- c)

conv :: Int -> Float
conv n = fromIntegral n / 100.0

centavosReais :: [Int] -> [Float]
centavosReais = map conv

-- QUESTÃO 3

-- a)

letras :: String -> String
letras = filter isLetter

-- b)

rmChar :: Char -> String -> String
rmChar c = filter (/= c)

-- c)

acima :: Int -> [Int] -> [Int]
acima n = filter (> n)

-- d)

diff x = fst x /= snd x

desiguais :: [(Int,Int)] -> [(Int,Int)]
desiguais = filter diff

-- QUESTÃO 4

-- a)
a' s = [toUpper c | c <- s, isAlpha c ]
a xs = map toUpper (filter isAlpha xs)

--b)

b' xs = [2 *x | x <- xs, x > 3 ]
b xs = map (*2) (filter (>3) xs)

--c)

c' strs = [reverse s | s <- strs, even (length s) ]

c :: [String] -> [String]
c xs = map reverse (filter (even . length) xs)

-- QUESTÃO 5

-- a)

productRec :: [Int] -> Int
productRec [] = 0
productRec [x] = x
productRec (x:xs) = x * productRec xs

productFold :: [Int] -> Int
productFold (x:xs) = foldr (*) x xs

-- b)

andRec :: [Bool] -> Bool
andRec [] = False
andRec [x] = x
andRec (x:xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold (x:xs) = foldr (&&) x xs

-- c)

concatRec :: [String] -> String
concatRec [] = ""
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [String] -> String
concatFold (x:xs) = foldr (++) x xs