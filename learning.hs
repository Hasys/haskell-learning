--1. Find the last element of a list.
myLast :: [a] -> a
myLast = head.reverse

myLastBetter :: [a] -> a
myLastBetter [x] = x
myLastBetter (_:xs) = myLastBetter xs

--2. Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

--3. Find the K'th element of a list.
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) i = elementAt xs (i-1)
--Error handling
elementAt _ _ = error "Index out of bound."

--4. Find number of elements of a list.
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

--5. Reverse a list.
myReverse :: [a] -> [a]
myReverse y = mReverse y [] where
						mReverse [] x = x
						mReverse (x:xs) a = mReverse xs (x:a)

myReverseAnother [] = []
myReverseAnother (x:xs) = myReverseAnother xs ++ [x]

--6. Find out whether a list is a palindrome.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome x = ( head x == last x ) && ( isPalindrome ( tail (init x) ) )
--Better function
--isPalindrome xs = (head xs) == (last xs) && (isPalindrome $ init $ tail xs)

--7. Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]
--My code starts here.
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List [a]) = flatten a
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

--8. Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress (x:xs) = 
				compress' xs x [] where
					compress' [] y compressed = compressed++[y]
					compress' (x:xs) y compressed = if x==y 
						then compress' xs y compressed
						else compress' xs x (compressed++[y])

--9. Pack consecutive duplicates of list elements into sublists. 
--If a list contains repeated elements they should be placed in separate sublists.
pack :: Eq a => [a] -> [[a]]
pack (x:xs) = 
			pack' xs x [x] [] where
				pack' [] _ z packed = packed++[z]
				pack' (x:xs) y z packed = if x==y
					then pack' xs y (z++[x]) packed
					else pack' xs x [x] (packed++[z])

--10. Consecutive duplicates of elements are encoded as lists (N E) where N is the 
--number of duplicates of the element E.
encode :: Eq a => [a] -> [(Int, a)]
encode (x:xs) =
				reverse (encode' xs x 1 []) where --I don't know why it is reversed at the end. :(
					encode' [] y n encoded = encoded++[(n, y)]
					encode' (x:xs) y n encoded  --Little bit another form of function.
						| x==y = encode' xs y (n+1) encoded
						| otherwise = encode' xs x 1 encoded++[(n, y)]

--11. Modified run-length encoding.
data Times a =  Single a 
				| Multiple Int a 
				deriving Show

encodeModified :: Eq a => [a] -> [(Times a)]
encodeModified x = reverse (enc (encode x) []) where
							enc [] res = res
							enc ( (n, chr):xs ) res
								| n==1 = enc xs res++[(Single chr)]
								| otherwise = enc xs res++[(Multiple n chr)]

--12. Decode a run-length encoded list.
decodeModified :: Eq a => [Times a] -> [a] --Eq a is not requaired, but it will be like reminder to be better :)
decodeModified [] = []
decodeModified (x:xs) = dec x ++ decodeModified xs where
							dec (Single a) = [a]
							dec (Multiple 1 a) = dec (Single a)
							dec (Multiple n a) = [a]++dec (Multiple (n-1) a)

decodeModifiedBetter :: [Times a] -> [a]
decodeModifiedBetter = concatMap decodeHelper where
						decodeHelper (Single x)     = [x]
						decodeHelper (Multiple n x) = replicate n x

--13. Run-length encoding of a list (direct solution).
encodeDirect :: Eq a => [a] -> [(Times a)]
encodeDirect (x:xs) = 
						reverse (enc xs x 1 []) where --Againe the same situation.
							enc [] y n encoded
								| n==1 = encoded++[(Single y)]
								| otherwise = encoded++[(Multiple n y)]
							enc (x:xs) y n encoded
								| x==y = enc xs y (n+1) encoded
								| otherwise = if n==1
									then enc xs x 1 encoded++[(Single y)]
									else enc xs x 1 encoded++[(Multiple n y)]

--14. Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

--15. Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = replicate n x ++ repli xs n

repliBetter :: [a] -> Int -> [a]
repliBetter xs n = concatMap (replicate n) xs

--16. Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = reverse (drop xs n (n-1) []) where
					drop [] _ _ x = x
					drop (x:xs) n i res
						| i==0 = drop xs n (n-1) res
						| otherwise = drop xs n (i-1) res++[x]

--17. Split a list into two parts; the length of the first part is given.
split :: [a] -> Int -> ([a], [a])
split xs n = spl [] xs n where
				spl first second 0 = (first, second)
				spl first (x:xs) n = spl (first++[x]) xs (n-1)

splitBest :: [a] -> Int -> ([a], [a])
splitBest xs n = (take n xs, drop n xs)

--18. Extract a slice from a list.
slice :: [a] -> Int -> Int -> [a]
slice xs i n = take (n-i+1) (drop (i-1) xs)

--19. Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate xs i = rt xs i where
				rt xs i 
					| i>=0 = (drop i xs)++(take i xs)
					| i<0 = (drop ( (length xs)-(0-i) ) xs ) ++ (take ( (length xs)-(0-i) ) xs)

--20. Remove the K'th element from a list.
removeAt :: Int -> [a] -> [a]
removeAt i xs = rm i xs where
					rm _ [] = []
					rm 1 (x:xs) = xs
					rm i (x:xs) = [x]++rm (i-1) xs
