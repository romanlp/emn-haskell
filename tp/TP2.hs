

-- Questions ?












-- on generalise (autant que possible) le type des fonctions du bloc1

myHead :: [a] -> a
myHead (x:_) = x

myTail :: [a] -> [a]
myTail (_:xs) = xs

myAppend :: [a] -> [a] -> [a]
myAppend xs ys = myAppend' xs 
    where --myAppend' :: [Int] -> [Int]
          myAppend' (x:xs) = x:myAppend' xs
          myAppend' [] = ys  

myInit :: [a] -> [a]
myInit [_] = []
myInit (x:xs) = x:(myInit xs)

myLast :: [b] -> b
myLast [x] = x
myLast (_:xs) = myLast xs

myNull :: [a] -> Bool
myNull [] = True
myNull _ = False

myLength :: [a] -> Int
myLength (_:xs) = 1 + myLength xs
myLength [] = 0

myReverse :: [a] -> [a]
myReverse (x:xs) = myAppend (myReverse xs) [x]
myReverse xs = xs

myConcat :: [[a]] -> [a]
myConcat (xs:xss) = xs ++ myConcat xss
myConcat [] = []

myTake :: Int -> [a] -> [a]
myTake 0 _  = []
myTake n [] = []
myTake n (x:xs) = x:myTake (n-1) xs

myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop n [] = []
myDrop n (x:xs) = myDrop (n-1) xs

myBangBang :: [a] -> Int -> a
myBangBang (x:xs) 0 = x
myBangBang (x:xs) n = myBangBang xs (n-1)

myInsert :: Ord a => a -> [a] -> [a]
myInsert x [] = [x]
myInsert x (y:ys) | x>y       = y:myInsert x ys
                  | otherwise = x:y:ys

mySort :: Ord a => [a] -> [a]
mySort (x:xs) = myInsert x (mySort xs)
mySort [] = []

-- NEW STUFF

-- ordre superieur

add :: Int -> Int -> Int
add x y = x + y

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile pred (x:xs) | pred x    = x:myTakeWhile pred xs
                        | otherwise = []
myTakeWhile pred []                 = []

-- donner le type de la fonction, notation infixe versus prefixe
myCompose :: (b -> c) -> (a -> b) -> a -> c 
myCompose f g x = f (g x)

myMap :: (a -> b) -> [a] -> [b]
myMap f (x:xs) = f x:myMap f xs
myMap f []     = []

test1 = myMap odd [1..10]

-- calcul des sous liste en utilisant map

sousListes :: [a] -> [[a]]
sousListes (x:xs) = myMap (x:) (sousListes xs) ++ sousListes xs
sousListes []     = [[]]

-- une fonction plus generale: foldr
-- inferer le type de foldr
-- forme graphique de la liste en peigne
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f k (x:xs) = f x (myFoldr f k xs)
myFoldr f k []     = k

myAnd' :: [Bool] -> Bool
myAnd' = foldr (&&) True

-- definir reverse avec foldr
myReverse' :: [a] -> [a]
myReverse' = undefined

-- une parenthese sur les lambda anonymes

add' :: Int -> Int -> Int
add' x y = x + y

add'' :: Int -> Int -> Int
add'' = \x y -> x + y

-- avec foldr
myReverse'' :: [a] -> [a]
myReverse'' = undefined

-- eta reduction
myReverse''' :: [a] -> [a]
myReverse''' = undefined

-- un "nouveau type" String

s1 :: String
s1 = "tout un tas de Char"

--type String = [Char]

-- un nouveau type tuples

myFst :: (a,b) -> a
myFst (x,_) = x

-- TODO: definir recursivement

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile f (x:xs) | f x 		 = myDropWhile f xs
			   		 | otherwise = x:xs
myDropWhile f []                 = []

myElem :: Eq a => a -> [a] -> Bool
myElem x [] 				= False
myElem x (y:ys) | x==y 		= True
				| otherwise = myElem x (ys)


myNotElem :: Eq a => a -> [a] -> Bool
myNotElem x (y:ys) = not (myElem x (y:ys))

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f (x:xs) | f x 		= x:myFilter f xs
				  | otherwise 	= myFilter f xs
myFilter f [] 					= []

mySplitAt :: Int -> [a] -> ([a],[a])
mySplitAt x (y:ys) = (myTake x (y:ys), myDrop x (y:ys))

myZip :: [a] -> [b] -> [(a,b)] 
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x,y):myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c] 
myZipWith f _ [] 		 	= []
myZipWith f [] _			= []
myZipWith f (x:xs) (y:ys) 	= (f x y):myZipWith f xs ys

myCurry :: ((a,b) -> c) -> a -> b -> c
myCurry f x y = f(x,y)

myUncurry :: (a -> b -> c) -> (a,b) -> c
myUncurry f (x,y) = f x y

myZipWith' :: (a -> b -> c) -> [a] -> [b] -> [c] 
myZipWith' = undefined

myUnzip :: [(a,b)] -> ([a],[b])
myUnzip = undefined

-- TODO: redefinir en utilisant foldr

myConcat' :: [[a]] -> [a]
myConcat' = undefined

myMap' ::  (a -> b) -> [a] -> [b]
myMap' = undefined

myOr' ::  [Bool] -> Bool
myOr' (x:xs) = myFoldr (||) x xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny = undefined

myAll :: (a -> Bool) -> [a] -> Bool
myAll = undefined

myProduct :: [Int] -> Int
myProduct (x:xs) = myFoldr (*) x xs

-- TODO: calculuer les 50 plus petits nombres premiers 2, 3, 5, 7, 11...

premiers :: [Int]
premiers = [2..]

test2 = take 50 (crible premiers)
	where crible (n:ns) = n:crible (filter (\x -> x `mod` n /= 0) ns)
