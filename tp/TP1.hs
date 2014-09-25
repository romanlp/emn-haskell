

{-
{-

plusieurs lignes 

-- commentaire

-}
-}

--import Data.List

cte1 :: Int
cte1 = 2+3*4

cte2 :: Int
cte2 = (2+3)*4

cte3 :: Int
cte3 = 2+(3*4)

mySub :: Int -> Int -> Int
mySub x y = x - y

un :: Int
un = 1

c1 :: Int
c1 = 1 - 3

c2 :: Int
c2 = (-) 1 3

c3 :: Int
c3 = mySub 1 3 

c4 :: Int
c4 = 1 `mySub` 3 

neg :: Int -> Int
neg = mySub 0

neg' :: Int -> Int
neg' y = mySub 0 y

titi :: Int -> Int -> Int
titi x y = mySub x y

neg'' :: Int -> Int -> Int
neg'' y x = mySub x y

neg''' :: Int -> Int
neg''' y = mySub y 0

l0 :: [Int]
l0 = []

l1 :: [Int]
l1 = 1:[]

l2 :: [Int]
l2 = 2:1:[]

l3 :: [Int]
--l3 = [2,4,6]
--l3 = [1..10]
--l3 = [10,9..1]
--l3 = [1,3..10]
l3 = 1:[3..10]

myHead :: [Int] -> Int
myHead (x:_) = x
--myHead (x:xs) = x

myTail :: [Int] -> [Int]
myTail (_:xs) = xs

b1 :: Bool
b1 = not (True && False)

b2 :: Bool
b2 = 1==2

-- ++
append1 :: [Int] -> [Int] -> [Int]
append1 (x:xs) ys = x:append1 xs ys
append1 []     ys = ys

append2 :: [Int] -> [Int] -> [Int]
append2 xs ys | not (null xs) = head xs:append2 (tail xs) ys
              | otherwise     = ys

append3 :: [Int] -> [Int] -> [Int]
append3 (x:xs) ys = let suite = append3 xs ys 
                    in x:suite
append3 []     ys = ys

append4 :: [Int] -> [Int] -> [Int]
append4 (x:xs) ys = x:suite where  suite = append4 xs ys 
append4 []     ys = ys

append5 :: [Int] -> [Int] -> [Int]
append5 xs ys = append5' xs
    where append5' (x:xs) = x:append5' xs
          append5' []     = ys


--TP
myInit :: [Int] -> [Int]
myInit (x:xs) | not (null xs) = x:myInit xs
myInit (x:[])				  = [] 

myLast :: [Int] -> Int
myLast (x:xs) | not (null xs) = myLast xs
myLast (x:[])				  = x

myNull :: [Int] -> Bool
myNull [] = True
myNull (x:xs) = False

myLength :: [Int] -> Int
myLength (x:xs) | not (null xs) = 1+myLength xs
myLength ([])				  = 0

myReverse :: [Int] -> [Int]
myReverse (x:[]) = (x:[])
myReverse xs = last xs : myReverse (init xs)

myConcat :: [[Int]] -> [Int]
myConcat (xs:ys) = xs++ myConcat ys
myConcat [] = []

myAnd :: [Bool] -> Bool
myAnd (x:xs) = x && myAnd(xs)
myAnd [] = True

myTake :: Int -> [Int] -> [Int]
myTake 0 (x:xs) = []
myTake n [] 	= []
myTake n (x:xs) = x : myTake (n-1) xs

myDrop :: Int -> [Int] -> [Int]
myDrop 0 (x:xs) = (x:xs)
myDrop n [] 	= []
myDrop n (x:xs) = myDrop (n-1) xs