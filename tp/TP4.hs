-- questions ?

l0 :: [Int]
l0 = 1:2:[]

data List = Cons Int List | Nil deriving Show

l1 :: List
l1 = Cons 1 (Cons 2 Nil)

data List' a = Cons' a (List' a) | Nil' deriving Show

l1' :: List' Int
l1' = Cons' 1 (Cons' 2 Nil')


--data Bin a = Node (Bin a) (Bin a) | Leaf a deriving Show

data Rose a = Rose a [Rose a] deriving Show

data Tree a = Fork (Tree a) a (Tree a) | Empty deriving Show

data Tree' a b = Fork' (Tree' a b) a (Tree' a b) | Empty' b deriving (Show,Read)

type MonArbre = Tree' Char Int

t1 :: MonArbre
t1 = Fork' (Empty' 2) 'a' (Empty' 1)

s1 :: String
s1 = "Fork' (Empty' 2) 'a' (Empty' 1)"

t2 :: MonArbre
t2 = read s1









-- data List = ...





-- NOM : 
-- PRENOM : 


-- compression de huffman

exemple1 :: String
exemple1 = "abcbcaa"

--exemple2 :: Huff (a,Int)
--exemple2 = (Node (Leaf ('a',3)) (Node (Leaf ('b',2)) (Leaf ('c',2)))) 

-- arbre de Huffman

data Huff a = Leaf a | Node (Huff a) (Huff a) deriving (Show,Eq)

-- chemin dans un arbre

type Bits = [Bit]

data Bit = L | R deriving (Show,Eq)

-- construction de l'arbre de Huffman

-- Q1 (def recursive)
-- calcule pour chaque caractere d'une chaine son nombre d'occurences
-- nbOccurrences exemple1 == [('a',3),('b',2),('c',2)]
nbOccurrences :: String -> [(Char,Int)]
nbOccurrences [] = []
nbOccurrences (c:cs) = (c, length (filter(==c) cs)+1): nbOccurrences (filter(/=c) cs)


-- Q2 (def recursive)
-- le poids est la somme des occurences contenues dans ses feuilles
-- poids (Node (Leaf ('a',3)) (Node (Leaf ('b',2)) (Leaf ('c',2)))) == 7
poids :: Huff (a,Int) -> Int
poids (Leaf (_,v)) = v
poids (Node x y) = poids x + poids y


-- Q3 (def recursive)
-- insere un arbre a sa place dans une liste d'arbres tries par poids croissants
-- insere (Leaf ('a',3)) [Leaf ('b',2),Leaf ('c',2)] == [Leaf ('b',2),Leaf ('c',2),Leaf ('a',3)]
insere :: Huff (a,Int) -> [Huff (a,Int)] -> [Huff (a,Int)] 
insere x [] = [x]
insere a1 (a2:a3) | poids a1 > poids a2 = a2:insere a1 a3
                  | otherwise 			= a1:a2:a3

-- Q4 (def non recursive avec foldr)
-- tri des arbres de Huffman par poids croissants
-- triHuff [Leaf ('a',3),Leaf ('b',2),Leaf ('c',2)] == [Leaf ('b',2),Leaf ('c',2),Leaf ('a',3)]
triHuff :: [Huff (a,Int)] -> [Huff (a,Int)]
triHuff xs = foldr insere [] xs

-- Q5 (def non recursive)
-- construit la liste des feuilles pour une chaine donnee
-- feuilles exemple1 == [Leaf ('b',2),Leaf ('c',2),Leaf ('a',3)]
feuilles :: String -> [Huff (Char,Int)]
feuilles (s) = triHuff (map Leaf (nbOccurrences s))

-- Q6 (def recursive)
-- agreege les deux arbres les plus legers, repetitivement pour obtenir un arbre unique
-- agrege [Leaf ('b',2),Leaf ('c',2),Leaf ('a',3)] == Node (Leaf ('a',3)) (Node (Leaf ('b',2)) (Leaf ('c',2)))
agrege :: [Huff (a,Int)] -> Huff (a,Int)
agrege [x] = x
agrege (x1:x2:xs) = agrege (insere (Node x1 x2) xs)

-- Q7 (def recursive)
-- efface les nombre d'occurences dans un arbre de Huffman
-- strip (Node (Leaf ('a',3)) (Node (Leaf ('b',2)) (Leaf ('c',2)))) == Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))
strip :: Huff (a,Int) -> Huff a
strip (Leaf (a,_)) = Leaf a
strip (Node x y) = Node (strip x) (strip y)

-- construit l'arbre de Huffman pour une chaine donnee
-- buildHuff exemple1 = Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))
buildHuff :: String -> Huff Char 
buildHuff = strip . agrege . feuilles


-- codage d'une chaine

-- code un caractere 
-- codeOne (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) 'b' == [R,L]
codeOne :: Huff Char -> Char -> Bits
codeOne root c = head (codeOne' root c)

-- Q8 (def recursive qui utilise aussi map)
-- code un caractere, retourne une liste de longueur 1 qui contient le chemin du caractere
--  codeOne' (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) 'b' == [[R,L]]
codeOne' :: Huff Char -> Char -> [Bits]
codeOne' (Leaf a) char 	| a == char = [[]]
  						| otherwise = []
codeOne' (Node g d) char = map (L:) (codeOne' g char) ++ map (R:) (codeOne' d char)

-- code tous les caracteres d'une chaine
-- codeAll (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) exemple1 == [L,R,L,R,R,R,L,R,R,L,L]
codeAll :: Huff Char -> String -> Bits
codeAll root s = concat (map (codeOne root) s)


-- Q9 (def recursive)
-- decodage d'une liste de bits
-- decode (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) [L,R,L,R,R,R,L,R,R,L,L] == "abcbcaa"
decode :: Huff Char -> Huff Char -> Bits -> String
decode = undefined

-- verifie la correction en codange puis decodant et calcule le ratio de compression (hors arbre)
-- test10 exemple1 == (True,0.19642857)
test10 :: String -> (Bool,Float)
test10 s = 
    let t = buildHuff s
        c = codeAll t s
        s' = decode t t c
    in (s==s',(fromIntegral (length c))/(fromIntegral (8*length s)))

-- Q10 
-- en 10 mots maximum identifier ce qui est recalcule de nombreuses fois et pourrait etre optimise dans ce code

-- reponse : 
