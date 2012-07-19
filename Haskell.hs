module Main (

    main
) where

import Data.List
import System.Random 
main = --print( quicksort [1,23,12,123,1,313,134,1,31,133,75,54,34668,7,92])
	   --print( divByTen 200)
	   --print( isUpperAlpha 'H')
	   --print( applyTwice (++ " HA HA") "Hey! " )
	   --print( applyTwice (3:) [1] )
	   --print( 1 * 2 )
	   --print( filter' (>=3) [1,2,3,4,5] ) 
	   --print( quicksort [1,2434,23,2,5,65,52,465])
	   --print( quicksort' [1,2434,23,2,5,65,52,465])
	   --print( zipWith' (\a b -> a + b) [1,2,3,4,5] [5,4,3,2,1] )
	   --print( problem1 ['x', 'v', 'z'] ) 
	   --print( elementAt (20) [1..5] )
	   --print( myLength  [1,2,3,4,5,6,7,8,9] )
	   --print( isPalindrome [1,2,2,1] )
	   --print( sumList [1,2,3,4,5,6,7])
	   --print( maximumFold [2,242,12,14,2134,234,24211,123,23,4,2222])
	   --print( reverseFold [1,2,3,4,5,6])
	   --print( map ($ 3) [(+4), (10*), (^2), sqrt] )
	   --print( groupProblem "aaaabccaadeeee")
	   --print( groupThem "aaaabccaadeeee")
	   --print( flattenList (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]))
	   --print( Main.splitAt 3 "aaannnsdsdsd")
	   --print( dropNth 3 "abcdefghik")
	   --print( getSlice 1 2 "123456789")
	   --print( replicateElements 3 "abc")
	   --print( rotateList (2) "1234567")
	   --print( isPrime 4)
	   --print( insertAt 2 'f' "123456789")
	   --print( rangeInt 4 9)
	   --print( sortList ["sssssssss","asda","aa", "ssss", "ssssss"] )
	   --print( sortListMod ["12345","aaa", "aa", "aaaa", "1234", "12", "123"])
       --print( decodeEncoded [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e'])
	   --print( modDecodeEncode "aaaabbbcceeeeklkkllllo")
	   --print( randomChoose 2 [1,2,3,4])
	   --print( randomPermutation [1,2,3,4])
	   --print( table3 (\a b c -> (and' c (or' a b))))
	     print( combination 3 "12345")
	 
lucky :: (Integral a) => a -> String
lucky 7 = "Lucky Number"
lucky x = "Not Lucky at all Sorry"

factorial :: (Integral a) => a -> a
factorial 1 = 1
factorial n = if n > 1 then n * (factorial (n - 1)) else 0
	
or' :: Bool -> Bool -> Bool
or' a b = or [a,b]

and' :: Bool -> Bool -> Bool
and' a b = and [a,b]

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + (length' xs)

bmTell :: (RealFloat a) => a -> String
bmTell bmi  
	| bmi <= 18.0 = "Hi 1"
	| bmi <= 25.0 = "Hi 2"
	| otherwise = "Hooo"

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b | a < b = LT | a == b = EQ | otherwise = GT

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = let sideArea = 2 * 3.14 * h
		   topArea  = 3.14 * r ^ 2
		in sideArea + 2 * topArea 


maximum' :: (Ord a) => [a] -> a
maximum' [] = error "must have length > 0"
maximum' [x] = x
maximum' (x:xs) 
	| x >= maxTail = x
	| otherwise = maxTail
	where maxTail = maximum' xs
 
replicate' :: (Num a, Ord a) => a -> b -> [b]
replicate' 0 _ = []
replicate' x xs = xs:replicate' (x-1) xs 


take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _ 
         | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs


zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = let first = quicksort [a | a <- xs, a <= x]
		       second = quicksort [a | a <- xs, a > x]
		   in  first ++ [x] ++ second

divByTen :: (Floating a) => a -> a
divByTen = (/10)

isUpperAlpha :: Char -> Bool
isUpperAlpha = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _  [] _ = []
zipWith' _  _ [] = []
zipWith' f (x:xs) (y:ys) =  (f x y) : zipWith' f xs ys

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) 
		| f x = x : filter' f xs 
		| otherwise =filter' f xs


quicksort2 :: (Ord a) => [a] -> [a]    
quicksort2 [] = []    
quicksort2 (x:xs) =     
        let smallerSorted = quicksort2 (filter (<=x) xs)  
            biggerSorted = quicksort2 (filter (>x) xs)   
        in  smallerSorted ++ [x] ++ biggerSorted  

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =  let  smallerSorted = quicksort' (filter' (<x) xs); 
						  biggerSorted = quicksort' (filter' (>=x) xs)
					 in   smallerSorted ++ [x] ++ biggerSorted
-- Using foldl and foldr	
sumList = foldl (+) 0

maximumFold :: (Ord a) => [a] -> a
maximumFold = foldl1 (\acc x -> if x >= acc then x else acc)

reverseFold :: [a] -> [a]
reverseFold = foldl (\acc x -> x:acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

------------------------------------------------
--Haskell 99 problem  url : http://www.haskell.org/haskellwiki/99_questions
--Helper Functions
removeFirst :: Int -> [b] -> [b]
removeFirst _ [] = []
removeFirst 1 (x:xs) = xs
removeFirst n (x:xs) = removeFirst (n-1) xs

takeFirst :: Int -> [b] -> [b]
takeFirst _ [] = []
takeFirst 1 (x:xs) = [x]
takeFirst n (x:xs) = x:takeFirst (n-1) xs

takeLast :: Int -> [b] -> [b]
takeLast _ [] = []
takeLast n x = removeFirst ((length' x) - n) x

-- Problem 1,2
-- Find the last element of a list. 
problem1 :: [a] -> a
problem1 [] = error "empty list"
problem1 [x] = error "length of list < 2"
problem1 [x1,x2] = x1
problem1 (x:xs) = problem1 xs

-- Problem 3
elementAt :: (Num a, Ord a) => a -> [b] -> b
elementAt _ [] = error "index larger then the list size"
elementAt x (y:ys)
		| x <= 0  = error "index less than or equal 0"
		| x == 1  = y
        | otherwise = elementAt (x-1) ys 

-- Problem 4
myLength :: (Num b) => [a] -> b
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

--problem 5
--Reverse a list. 
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = (reverseList xs) ++ [x]

--problem 6
--Palindrome
isPalindrome :: (Ord a) => [a] -> Bool
isPalindrome [] = True
isPalindrome[x] = True
isPalindrome x = (reverseList x) == x

--problem 7
data NestedList a = Elem a | List [NestedList a]
flattenList ::  NestedList a -> [a]
flattenList (Elem x) = [x]
flattenList (List (x:xs)) = flattenList x ++ flattenList (List xs)
flattenList (List []) = []

--problem 8
--Eliminate consecutive Duplicates
eleminateProblem :: (Eq a) => [a] -> [a]
eleminateProblem [] = []
eleminateProblem [x] = [x]
eleminateProblem (x1:x2:xs) 
				| (x1 == x2) = eleminateProblem (x1:xs)
				| otherwise = x1 : eleminateProblem (x2:xs)

--problem 9
--Group consecutive Duplicates
groupThem :: (Eq a) => [a] -> [[a]]
groupThem [] = []
groupThem [x] = [[x]]
groupThem all@(x:xs) =  let number = length'(takeWhile (== x) all);
						newList = removeFirst number all
					in 	(replicate' number x) : (groupThem newList)
							
--problem 10
--Run-length encoding of a list
groupProblem :: [Char] -> [(Char,Int)]
groupProblem [] = []
groupProblem [x] = [(x, 1)]
groupProblem (x:xs) =  let number = length(takeWhile (== x) (x:xs));
						   newList = removeFirst number (x:xs)
						in 	(x, number):groupProblem newList

--Problem 11
--(*) Modified run-length encoding. 
groupProblemModified :: (Eq a) => [a] -> [(a,Int)]
groupProblemModified [] = []
groupProblemModified [x] = [(x, 1)]
groupProblemModified (x:xs) =  let number = length(takeWhile (== x) (x:xs));
						           newList = removeFirst number (x:xs)
						       in  (x, number):groupProblemModified newList
	
--Problem 12
--(**) Decode a run-length encoded list. 					
data Encoded = Multiple Int Char | Single Char  deriving (Show)
decodeEncoded :: [Encoded] -> [Char]
decodeEncoded [] = []
decodeEncoded ((Multiple x ch):xs) = (replicate' x ch) ++ decodeEncoded xs
decodeEncoded ((Single ch):xs)	= ch:decodeEncoded xs
 
--Problem 13
--(**) Run-length encoding of a list (direct solution). 
modDecodeEncode :: [Char] -> [Encoded]
modDecodeEncode [] = []
modDecodeEncode x = let newList = groupProblem x;
						modifiedEncoding [] = [];
						modifiedEncoding ((a,b):xs) = (if(b > 1) then (Multiple b a):modifiedEncoding xs else (Single a):modifiedEncoding xs)
					in modifiedEncoding newList
--Problem 14	
--(*) Duplicate the elements of a list. 
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x:x:duplicate xs

--Problem 15
--(**) Replicate the elements of a list a given number of times.
replicateElements :: Int -> [a] -> [a]
replicateElements _ [] = []
replicateElements n (x:xs) = (replicate' n x) ++ replicateElements n xs

--Problem 16
--(**) Drop every N'th element from a list. 
dropNth :: Int -> [b] -> [b]
dropNth _ [] = []
dropNth n (x:xs) = let  dropNthNumber :: Int -> Int -> [b] -> [b];
					    dropNthNumber _ _ [] = [];
						dropNthNumber k n (x:xs)
								| (k `mod` n == 0) = dropNthNumber (k + 1) n xs
								| otherwise = x : dropNthNumber (k + 1) n xs 
				   in dropNthNumber 1 n (x:xs)

--Problem 17
--(*) Split a list into two parts; the length of the first part is given. 							
splitAt :: Int -> [b] -> [[b]]
splitAt _ [] = []
splitAt 0 xs = [xs]
splitAt n xs = [(takeFirst n xs), (removeFirst n xs)]

--Problem 18
--(**) Extract a slice from a list. 
getSlice :: Int -> Int -> [b] -> [b]
getSlice _ _ [] = []
getSlice a b (x:xs) 
			| (b <= 0)  = []
			| (a == 1)  = x:getSlice a (b - 1) xs
			| otherwise = getSlice (a - 1) (b - 1) xs

--Problem 19
--(**) Rotate a list N places to the left. 
rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n xs
			| (n < 0) = (takeLast (abs n) xs) ++ (takeFirst (length xs - (abs n)) xs)
			| (n > 0) = (removeFirst n xs) ++ (takeFirst n xs)
			| otherwise = xs

--Problem 20
--(*) Remove the K'th element from a list. 
removeAt :: (Integral a) => a -> [b] -> [b]
removeAt _ [] = []
removeAt 1 (x:xs) = xs
removeAt n (x:xs) = x:removeAt (n-1) xs

--Problem 21
--Insert an element at a given position into a list. 
insertAt :: Int -> a -> [a] -> [a]
insertAt i x all@(y:ys) 
			| (i <= 0) = error "invalid index Range"
			| (i == 1) = x:all
			| otherwise = y:(insertAt (i - 1) x ys)

--Problem 22
--Create a list containing all integers within a given range. 
rangeInt :: Int -> Int -> [Int]
rangeInt a b 
		| (a > b) = error "invalid Range between (a,b)"
		| (a == b) = [b]
		| otherwise = a:(rangeInt (a+1) b) 

--Problem 23
--Extract a given number of randomly selected elements from a list. 
seed = (mkStdGen 100)
randomStuff :: RandomGen g => g -> Int -> Int -> Int
randomStuff seed a b = fst (randomR (a, b) seed)

randomChoose :: Int -> [a] -> [a]
randomChoose _ [] = []
randomChoose n xs
				| (n <= 0) = []
				| length xs == 0 = error "number of needed no >>> length of the list"
				| otherwise =  let index = randomStuff seed 1 (length xs)--randomRs (0, length xs - 1) (mkStdGen 100)
							   in (elementAt index xs):(randomChoose (n-1) (removeAt index xs))

--Problem 24
--Lotto: Draw N different random numbers from the set 1..M. 
randomChooseRange :: Int -> Int -> [Int]
randomChooseRange i range = randomChoose i [1..range]

--Problem 25
--Generate a random permutation of the elements of a list. 
randomPermutation [] = []
randomPermutation xs= randomChoose (length xs) xs

--Problem 26
--(**) Generate the combinations of K distinct objects chosen from the N elements of a list 
combination :: Int -> [a] -> [[a]]
combination _ [] = []
combination n ys = let  combinationHelper :: Int -> Int -> [a] -> [[a]];
						combinationHelper  _ _ [] = [];
						combinationHelper a b all@(x:xs)
								| (b - a) > length' all = []
								| (a == b) = [[x]]
								|otherwise = (map ([x]++) (combinationHelper (a+1) b xs)) ++ (combinationHelper a b xs)
				   in combinationHelper 1 n ys

--Problem 28
--Sorting a list of lists according to length of sublists
--a)		
sortList :: [[a]] -> [[a]]
sortList [] = []
sortList x = sortBy (\a b -> if (length a) <= (length b) then LT else GT) x
			 --sortBy (compare `on` length') x
--b)
sortPredicate = sortBy (\a b -> if (length a) > (length b) then GT else LT)
sortListMod :: [[a]] -> [[a]]
sortListMod [] = []
sortListMod x = concat (sortPredicate (groupBy (\a b -> length a == length b) ( sortPredicate x)))

--Problem 31
--(**) Determine whether a given integer number is prime. 
isPrime :: Int -> Bool
isPrime 1 = True
isPrime x = let isPrimeModified :: Int -> Int -> Bool;
				isPrimeModified k x
								|  x < k * k = True
								| (x `mod` k == 0) = False
								| otherwise = isPrimeModified (k+1) x
			in isPrimeModified 2 x

--Problem 46, Problem 47
--(**) Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 
generateTable :: [Bool] -> [Bool] -> (Bool -> Bool -> Bool) -> [Bool]
generateTable [] _ f = [];
generateTable _ [] f = [];
generateTable (x:xs) ys f = (map (f x) ys) ++ generateTable xs ys f;

table :: (Bool -> Bool -> Bool) -> [Bool]
table f = generateTable [True, False] [True, False] f

--Problem 48
--(**) Truth tables for logical expressions (3)
generateTable3 :: [Bool] -> [Bool] -> [Bool] -> (Bool -> Bool -> Bool -> Bool) -> [Bool]
generateTable3 [] _ _ f = [];
generateTable3 _ [] _ f = [];
generateTable3 _ _ [] f = [];
generateTable3 (x:xs) ys zs f = (generateTable ys zs (f x)) ++ (generateTable3 xs ys zs f)

table3 :: (Bool -> Bool -> Bool -> Bool) -> [Bool]
table3 f = generateTable3 [True, False] [True, False] [True, False] f



