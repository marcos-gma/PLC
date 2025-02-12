import Graphics.Win32 (pS_ALTERNATE)
-- QUESTÃO 1

-- a)

type ID = String
type Pessoa = String
type Equipe = [Pessoa]
type Projeto = (ID, Equipe)
type Projetos = [Projeto]

-- b)

criarProjeto :: ID -> Pessoa -> Projetos -> Projetos
criarProjeto id p ps 
    | null [x | x <- ps, fst x == id] = (id, [p]) : ps
    | otherwise = ps

criaProjetoR :: ID -> Pessoa -> Projetos -> Projetos
criaProjetoR x pessoa [] = [(x, [pessoa])] 
criaProjetoR x pessoa (a:as)
    | fst a == x = (a:as) 
    | otherwise = a : criaProjetoR x pessoa as 


-- c)

equipe :: ID -> Projetos -> Equipe
equipe id ps 
    | null [x | x <- ps, fst x == id] = []
    | otherwise = snd(head [x | x <- ps, fst x == id])

equipeR :: ID -> Projetos -> Equipe
equipeR id [] = []
equipeR id (a:as)
    | fst a == id = snd a
    | otherwise = equipeR id as


-- d)

naEquipe :: Projeto -> Pessoa -> Bool
naEquipe p ps 
    | null (filter (== ps) (snd p)) = False
    | otherwise = True

naEquipe2 :: Projeto -> Pessoa -> Bool
naEquipe2 p ps 
    | not (any (== ps) (snd p)) = False
    | otherwise = True

naEquipe3 :: Projeto -> Pessoa -> Bool
naEquipe3 p ps 
    | notElem ps (snd p) = False
    | otherwise = True

naEquipeR :: ID -> Pessoa -> Projetos -> Bool
naEquipeR _ _ [] = False  
naEquipeR x y ((id, equipe):projetos)
    | x == id = y `elem` equipe 
    | otherwise = naEquipeR x y projetos

-- e)

acrescentarPessoa :: Pessoa -> ID -> Projetos -> Projetos
acrescentarPessoa p id [] = []
acrescentarPessoa p id (x:xs)
    | fst x == id = (id, snd x ++ [p]) : xs
    | otherwise =  x : acrescentarPessoa p id xs

-- QUESTÃO 2

data Nat  =  Zero | Succ  Nat  deriving  (Eq, Show) 

-- a)

int2Nat :: Int -> Nat
int2Nat 0 = Zero
int2Nat n
    | n > 0 = Succ (int2Nat (n - 1))
    | otherwise = error "Negative numbers are not allowed"

-- b)

nat2Int :: Nat -> Int
nat2Int Zero = 0
nat2Int (Succ n) = 1 + nat2Int n

-- c)

soma ::  Nat -> Nat -> Nat
soma Zero n = n
soma (Succ m) n = Succ (soma m n)

-- d)

somaInt :: Nat -> Nat -> Int
somaInt n m = nat2Int(soma n m)