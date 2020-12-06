{-
	These 50 questions are available at the beggining of the semester as an evaluation and study tool for students on the 1st year of the Integrated Master in Computing Engineering at Universidade do Minho, Braga

	These solutions were made 2 years after the conclusion of the matter by the author, made to remember Haskell's basics and syntax.
-}

--Creates a list that starts in x and ends in y with all elements in between x and y
myEnumFromTo :: Int -> Int -> [Int]
myEnumFromTo x y = [x..y]

--Creates a list os integers starting in x, with an offset of (x-o) until we get to y
myEnumFromThenTo :: Int -> Int -> Int -> [Int]
myEnumFromThenTo x o y = [x,o..y]

--Given two lists, concatenates both into a single list
myConcatA :: [a] -> [a] -> [a]
myConcatA [] l = l
myConcatA l [] = l
myConcatA (h:t) l = h:(myConcatA t l)

--Given a list and an index, returns the value of that index on the given list
myIndexedElement :: [a] -> Int -> a
myIndexedElement (h:t) 0 = h
myIndexedElement (h:t) i = myIndexedElement t (i-1)

--Given a list, reverse the order of it's elements
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (h:t) = (myReverse t) ++ [h]

--Given a list and a number n we'll take the first n elements of that list.
myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n [] = []
myTake n (h:t) = h:(myTake (n-1) t)

--Given a list and a number n, drops the first n elements of the array
myDrop :: Int -> [a] -> [a]
myDrop 0 l = l
myDrop _ [] = []
myDrop n (h:t) = myDrop (n-1) t

--Given two lists of the same size, creates a list of doubles, where the first element comes from the 1st list and the second accordingly
myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (h1:t1) (h2:t2) = (h1,h2):(myZip t1 t2)

--Checks if an element exists in the list
myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x (h:t) | x == h = True
               | otherwise = myElem x t

--Given an element and a number n, creates a list of n elements of that element
myReplicate :: Int -> a -> [a]
myReplicate 0 x = []
myReplicate n x = x:(myReplicate (n-1) x)

--Given an element and a list, places that element between all elements of the list
myIntersperse :: a -> [a] -> [a]
myIntersperse x [a] = [a]
myIntersperse x (h:t) = h:x:(myIntersperse x t)

--Given a list, groups the consecutive equals elements in a list of lists
myGroup :: Eq a => [a] -> [[a]]
myGroup [] = []
myGroup l = take (auxGroup l) l : myGroup(drop(auxGroup l) l)

auxGroup :: Eq a => [a] -> Int
auxGroup [a] = 1
auxGroup (h:i:t) | h == i = 1 + auxGroup(i:t)
                 | otherwise = 1

--Given a list of lists, concatenates all lists into one
myConcat :: [[a]] -> [a]
myConcat [[]] = []
myConcat (h:t) = h ++ myConcat t

--Given a list, creates a list of inits
myInits :: [a] -> [[a]]
myInits [] = [[]]
myInits l = myInits (init l) ++ [l]

--Given a list, creates a list of tails
myTails :: [a] -> [[a]]
myTails [] = [[]]
myTails l = [l] ++ myTails (tail l)

--Given two lists, checks if the first is a prefix of the second
myIsPrefixOf :: Eq a => [a] -> [a] -> Bool
myIsPrefixOf [] _ = True
myIsPrefixOf (h1:t1) (h2:t2) | h1 == h2 = (myIsPrefixOf t1 t2)
                           | otherwise = False

--Given two lists, checks if the first is a suffix of the second
myIsSufixOf :: Eq a => [a] -> [a] -> Bool
myIsSufixOf [] _ = True
myIsSufixOf l1 l2 | (last l1) == (last l2) = myIsSufixOf (init l1) (init l2)
                  | otherwise = False

--Tests if the elements of a list occures on the other (on the same order)
myIsSubsequenceOf :: Eq a => [a] -> [a] -> Bool
myIsSubsequenceOf [] _ = True
myIsSubsequenceOf l [] = False
myIsSubsequenceOf (h1:t1) (h2:t2) | h1 == h2 = myIsSubsequenceOf t1 t2
                                  | otherwise = myIsSubsequenceOf (h1:t1) t2

--Creates a list with the indexes of all occurences of the element
myElemIndices :: Eq a => a -> [a] -> [Int]
myElemIndices x [] = []
myElemIndices x (h:t) = aux 0 x (h:t)
                      where aux a x [] = []
                            aux a x (h:t) | x == h = a:aux (a+1) x t
                                          | otherwise = aux (a+1) x t

--Given a list, returns the same list without repeted elements
myNub :: Eq a => [a] -> [a]
myNub [] = []
myNub (h:t) = h:myNub (aux h t)
          where aux h [] = []
                aux h (i:t) |h == i = aux h t
                            |otherwise = i:aux h t

--Given an element and a list, removes the first occurance of that element on that list
myDelete :: Eq a => a -> [a] -> [a]
myDelete x [] = []
myDelete x (h:t) | x == h = t
                 | otherwise = h:(myDelete x t)

--Given tow arrays, deletes from the first, the first occurance of the elements on the second
mySlash :: Eq a => [a] -> [a] -> [a]
mySlash l [] = l
mySlash l (h:t) | elem h l = mySlash (myDelete h l) t
                | otherwise = mySlash l t

--Given two lists, unites both (inexistence of repeated numbers)
myUnion :: Eq a => [a] -> [a] -> [a]
myUnion l [] = l
myUnion l (h:t) | elem h l = myUnion l t
                | otherwise = myUnion (l++[h]) t 

--Given two lists, intersects both
myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect [] l = []
myIntersect l [] = []
myIntersect l (h:t) | elem h l = h:(myIntersect l t)
                    | otherwise = myIntersect l t

--Given an ordered list and an element of it's type, inserts the element and returns the new list
myInsert :: Ord a => a -> [a] -> [a]
myInsert x [] = [x]
myInsert x (h:t) | x >= h = h:(myInsert x t)
                 | otherwise = x:h:t

--Given a list of Strings, puts them all on the same String separated by spaces
myUnwords :: [String] -> String
myUnwords [] = ""
myUnwords [h] = h
myUnwords (h:t) = h ++ " " ++ myUnwords t


--Given a list of Strings, puts them all on the same String but in different lines
myUnlines :: [String] -> String
myUnlines [] = ""
myUnlines (h:t) = h ++ "\n" ++ myUnlines t

--Given a list, returns the index of the biggest element on the list
myPMaior :: Ord a => [a] -> Int
myPMaior (h:t) = index (maxElem h t) (h:t)
                 where maxElem h (x:xs) | h > x = maxElem h xs
                                        | otherwise = maxElem x xs
                       index h (x:xs) | h == x = 0
                                      | otherwise = 1 + (index h xs)

--Given a list, checks if that list has repeated elements
myTemRepetidos :: Eq a => [a] -> Bool
myTemRepetidos [] = False
myTemRepetidos (h:t) | elem h t = True
                     | otherwise = myTemRepetidos t


--Given a list of char, returns the list of Chars that represent numbers
myAlgarismos :: [Char] -> [Char]
myAlgarismos [] = []
myAlgarismos (h:t) | (48 <= (fromEnum h) && (fromEnum h) <= 57) = h:(myAlgarismos t)
                   | otherwise = myAlgarismos t

--Given a list, returns the same list containing only all odd indexed elements
myPosImpares :: [a] -> [a]
myPosImpares [] = []
myPosImpares [x] = []
myPosImpares (h:i:t) = i:(myPosImpares t)

--Given a list, returns the same list containing only all even indexed elements
myPosPares :: [a] -> [a]
myPosPares [] = []
myPosPares [x] = [x]
myPosPares (h:i:t) = h:(myPosPares t)

--Given a list, tests if it's sorted or not
myIsSorted :: Ord a => [a] -> Bool
myIsSorted [] = True
myIsSorted [x] = True
myIsSorted (h:i:t) | h <= i = myIsSorted (i:t)
                   | otherwise = False

--Sorting algorithm that uses recorrent insertions to order the list
myISort :: Ord a => [a] -> [a]
myISort [] = []
myISort (h:t) = myInsert h (myISort t)

--Given two strings, returns True only if the first is smaller than the first (lexical analysis)
myMenor :: String -> String ->  Bool
myMenor h t | h<t = True
            | otherwise = False

--Given an MSet element and a list of MSet elements, determines if the given element belongs in the list
myElemMSet :: Eq a => a -> [(a,Int)] -> Bool
myElemMSet x [] = False
myElemMSet x ((a,i):t) | x == a = True
                       | otherwise = myElemMSet x t 

--Given a list of MSet elements, calculates the length of the MSet
myLengthMSet :: [(a,Int)] -> Int
myLengthMSet [] = 0
myLengthMSet ((a,i):t) = i + myLengthMSet t

--Given a list of MSet elements, converts in a list of it's elements
myConverteMSet :: [(a,Int)] -> [a]
myConverteMSet [] = []
myConverteMSet ((a,i):t) | i > 0 = [a] ++ (myConverteMSet ((a,i-1):t))
                         | otherwise = myConverteMSet t

--Given an element and a list of MSet elements, adds that element to the list
myInsereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
myInsereMSet x [] = [(x,1)]
myInsereMSet x ((a,i):t) | x == a = ((a,i+1):t)
                         | otherwise = [(a,i)] ++ (myInsereMSet x t)

--Given an element and a list of MSet elements, removes that element from the list
myRemoveMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
myRemoveMSet x [] = []
myRemoveMSet x ((a,i):t) | x == a = ((a,i-1):t)
                         | otherwise = [(a,i)] ++ (myRemoveMSet x t)

--Given a list of elements, builds an MSet with that list
myConstroiMSet :: Ord a => [a] -> [(a,Int)]
myConstroiMSet [] = []
myConstroiMSet l = aux l []
               where aux [] l  = l
                     aux (h:t) l = aux t (myInsereMSet h l)

--Given a list of Eithers, creates a pair wehre both elements are lists of the two possible ways of the Either
myPartitionEithers :: [Either a b] -> ([a],[b])
myPartitionEithers l = (lefts l, rights l)

lefts :: [Either a b] -> [a]
lefts [] = []
lefts (Left a:t) = a:(lefts t)
lefts (Right b:t) = (lefts t)

rights :: [Either a b] -> [b]
rights [] = []
rights (Right b:t) = b:(rights t)
rights (Left a:t) = (rights t)

--Given a list of Maybe a elements, creates a list with all type a elements
myCatMaybes :: [Maybe a] -> [a]
myCatMaybes [] = []
myCatMaybes (Just x:t) = x:(myCatMaybes t)
myCatMaybes (Nothing:t) = myCatMaybes t

data Movimento = Norte | Sul | Este | Oeste
               deriving Show

--Given an initial position and a list of Movements, calculates the final position
myPosicao :: (Int,Int) -> [Movimento] -> (Int,Int)
myPosicao x [] = x
myPosicao (x,y) (Norte:t) = myPosicao (x,y+1) t
myPosicao (x,y) (Sul:t) = myPosicao (x,y-1) t
myPosicao (x,y) (Este:t) = myPosicao (x+1,y) t
myPosicao (x,y) (Oeste:t) = myPosicao (x-1,y) t

--Given the initial and final position, calculates the list of Movements necessary to build the way between them
myCaminho :: (Int,Int) -> (Int,Int) -> [Movimento]
myCaminho (x1,y1) (x2,y2) | x1 < x2 = [Este] ++ (myCaminho (x1+1,y1) (x2,y2))
                          | x1 > x2 = [Oeste] ++ (myCaminho (x1-1,y1) (x2,y2))
                          | x1 == x2 && y1 < y2 = [Norte] ++ (myCaminho (x1,y1+1) (x2,y2))
                          | x1 == x2 && y1 > y2 = [Sul] ++ (myCaminho (x1,y1-1) (x2,y2))
                          | x1 == x2 && y1 == y2 = []

--Given a list of Movements, checks if they're all vertical
myVertical :: [Movimento] -> Bool
myVertical [] = True
myVertical (Norte:t) = myVertical t
myVertical (Sul:t) = myVertical t
myVertical (h:t) = False

data Posicao = Pos Int Int
             deriving Show

--Given a list of Positions checks wich one is closer to the center (0,0)
myMaisCentral :: [Posicao] -> Posicao
myMaisCentral [h] = h
myMaisCentral (h:i:t) = myMaisCentral ((centro h i):t)
                    where centro (Pos x y) (Pos m n) |(x^2 + y^2) < (m^2 + n^2) = Pos x y
                                                     |otherwise = Pos m n 

--Given a position and a list of positions, returns the list of neighbour positions to the given
vizinho :: Posicao -> [Posicao] -> [Posicao]
vizinho p [] = []
vizinho (Pos x y) ((Pos m n):t) | y == n && (m == x-1 || m == x+1) = (Pos m n):vizinho (Pos x y) t
                                | x == m && (n == y-1 || n == y+1) = (Pos m n):vizinho (Pos x y) t
                                | otherwise = vizinho (Pos x y) t

--Checks if all positions inside the given list have the same y coordinate value
myMesmaOrdenada :: [Posicao] -> Bool
myMesmaOrdenada [a] = True
myMesmaOrdenada ((Pos x y):(Pos m n):t) | y /= n = False
                                        | otherwise = myMesmaOrdenada ((Pos m n):t)

data Semaforo = Verde | Amarelo | Vermelho
              deriving Show

interseccaoOK ::  [Semaforo] -> Bool
interseccaoOK [] = True
interseccaoOK l = (naoVermelho l) < 2
              where naoVermelho [] = 0
                    naoVermelho (h:t) = case h of
                        Vermelho -> naoVermelho t
                        otherwise -> 1 + naoVermelho t