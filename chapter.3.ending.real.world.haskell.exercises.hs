-- 1. Write a function that computes the number of elements in a list. To test it, ensure that it gives the same answers as the standard length  function.

mySize :: [a] -> Int

mySize xs | null xs				= 0
mySize xs = 1 + mySize (tail xs)

-- 2. Add a type signature for your function to your source file. To test it, load the source file into ghci  again. (ALREADY DONE, BIATCH!)

-- 3. Write a function that computes the mean of a list, i.e. the sum of all elements in the list divided by its length. (You may need to use the fromIntegral function to convert the
--  length of the list from an integer into a floating point number.)

mySum :: [Double] -> Double

mySum xs | null xs 			= 0
mySum xs					= (head xs) + mySum (tail xs)

myMean :: [Double] -> Double

myMean xs | null xs			= 0
myMean xs 					= mySum xs / fromIntegral (mySize xs)

-- We interrupt these exercises to make note of the fact that I am a bad motherfucker!

-- 4. Turn a list into a palindrome, i.e. it should read the same both backwards and forwards. For example, given the list [1,2,3], your function should return [1,2,3,3,2,1].

-- learn a little about list pattern matching first:
getx (x:xs) = x
getxs (x:xs) = xs

hmmm (x:y:z:xs) = xs

-- Make my own last function
mylast :: [a] -> a

mylast (x:xs) | null xs				= x
mylast xs | null xs					= error "give a brother a break please"
mylast (x:xs)							= mylast xs

allbutlast :: [a] -> [a]

allbutlast (x:xs) | null xs 		= []
allbutlast (x:xs) | length xs == 1 	= [x]
allbutlast xs | null xs				= error "come on dude, be reasonable"
allbutlast (x:xs)					= x : allbutlast xs

rev :: [a] -> [a]

rev [] = []
rev (x:xs) | null xs			= [x]
rev (xs) 						= last xs : rev (allbutlast xs)

-- I know there's already a way to concatenate lists, but I'm getting good practice writing my own stuff here.
cat :: [a] -> [a] -> [a]

cat [] [] = []
cat x [] = x
cat [] y = y

cat (x:xs) (y) = x : cat xs y -- SWEET! I didn't realize that, once I hit Y, only one more : was needed! No need to "deconstruct" y!!

makePd :: [a] -> [a]

makePd [] = []
makePd (x:xs) | null xs			= [x]
makePd (xs)						= cat xs (rev xs)

-- WHHOOOOO HOOOOOOO!!!!!!!

-- 5. Write a function that determines whether its input list is a palindrome.

-- sample: elem :: (Eq a) => a -> [a] -> Bool

areListsEqual :: (Eq a) => [a] -> [a] -> Bool

areListsEqual [] [] 	= True
areListsEqual [] _ 		= False
areListsEqual _ []		= False

areListsEqual (x:xs) (y:ys)		= (x == y)  && (areListsEqual xs ys)

isPalindrome :: (Eq a) => [a] -> Bool

isPalindrome [] 				= True
isPalindrome (x:xs) | null xs	= True

isPalindrome xs					= areListsEqual xs (rev xs)

-- 6. Create a function that sorts a list of lists based on the length of each sublist. (You may want to look at the sortBy function from the Data.List module.)

getByIndex :: [a] -> Int -> a

getByIndex [] _ = error "dang, man, that list is empty"

getByIndex xs i | (i + 1) > (length xs)				= error ("But that list ain't long enough to get the item number you asked for")
getByIndex _ i | i < 0							= error "why you gon' be like dat?"

getByIndex xs i									= head (drop i xs)

doSwap :: [a] -> Int -> [a]

doSwap [] _ = []
doSwap (x:xs) _ | null xs						= [x]

doSwap xs index									= stuffBeforePair ++ tail thePair ++ [head thePair] ++ stuffAfterPair
													where
														stuffBeforePair = take index xs
														stuffAfterPair = drop (index + 2) xs
														thePair = take 2 (drop index xs)

mySort :: (Eq a) => [[a]] -> [[a]]

mySort [] 							= []
mySort [x]				 			= [x]
mySort xs@(first:second:theRest)			= if(not (areListsEqual xs wip))
									  	  then mySort wip 
									  	  else wip
									  	  where
											swapPairIfNeeded a b = if(length a >= length b) 
																   then (b, a)
																   else (a, b)
											(newFirst, newSecond) = swapPairIfNeeded first second
											wip = newFirst:mySort ( newSecond:theRest)
						
-- Yes, they gave a little hint about some sort function in the standard libs or something, but not enough info on how to use it. So I just
-- WENT MY OWN WAY! I didn't peek at jackshit! I just ROLLED MY OWN BUBLESORT 

soSort :: (Eq a) => [[a]] -> [[a]]
soSort []     = []
soSort (x:xs) = soSort (filter (cmpLen (>) x) xs) ++ [x] ++
                     soSort (filter (cmpLen (<=) x) xs)
   where filter _ [] = []
         filter p (x:xs) = (if (p x) then (x:) else id) (filter p xs)
         cmpLen f x y = f (length x) (length y)

-- 7. Define a function that joins a list of lists together using a separator value.
intersperse ::  a -> [[a]] -> [a]

-- intersperse '*' ["foo","bar","baz","quux"] 
--  should produce the following:
--  "foo*bar*baz*quux"

-- intersperse -99 [ [1,2,3],[4,5,6],[7,8,9]]
--  should produce the following:
--  [1,2,3,-99,4,5,6,-99,7,8,9]

-- intersperse _ [x] | null x = []
-- intersperse _ [x] = x
-- intersperse s (x:y:xs) = x:s:y:intersperse s xs

-- THIS WORKS!! intersperse s (x:y:xs) =  x ++ s:y ++ intersperse s ([y] ++ xs)
intersperse _ [] = []
intersperse _ [x] = x
intersperse s (x:xs) =  x ++ s:intersperse s xs 

-- I am learning that there is usually not the need for very many levels. You can find a way to get a simple operation right, and then use it over and over.

-- 8. Using the binary tree type that we defined earlier in this chapter, write a function that will determine the height of the tree. The height is the largest 
-- number of hops from the root to an Empty. For example, the tree Empty has height zero; Node "x" Empty Empty has height one;
--  Node "x" Empty (Node "y" Empty Empty) has height two; and so on.

-- first, here's their tree data type.
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

depth :: (Eq a) => Tree a -> Int

depth Empty = 0
depth (Node _ (left) (right)) = 1 + (max (depth left) (depth right) )

-- I knocked that one out pretty quickly.

-- 9. Consider three two-dimensional points a, b, and c. If we look at the angle formed by the line segment from a to b and the line segment from b to c, 
-- it either turns left, turns right, or forms a straight line. Define a Direction data type that lets you represent these possibilities. 
data TurnDirection = LeftTurn
				 | RightTurn
				 | Straight
	deriving (Show, Eq)
	
-- 10. Write a function that calculates the turn made by three 2D points and returns a TurnDirection (I changed name to TurnDirection)
data Point = Point {
	x	:: Double
	,y	:: Double
	} deriving (Show)
	
data Line = Line {
	startPoint :: Point
	, endPoint :: Point
	} deriving (Show)

data WhichCoordinate = X | Y
	deriving (Show, Eq)
	
comparisonCoord :: Line -> WhichCoordinate

comparisonCoord (Line (Point sx sy) (Point ex ey)) | sx == ex && sy == ey 		= error "Those 2 points are the same, which prevents me from defining a line"
comparisonCoord (Line (Point _ sy)  (Point _ ey)) | sy == ey 				 	= Y
comparisonCoord (Line (Point sx _)  (Point ex _)) | sx == ex 				 	= X
comparisonCoord _																= Y

other :: WhichCoordinate -> WhichCoordinate

other X = Y
other Y = X

directionalCoord :: Line -> WhichCoordinate
directionalCoord l = other (comparisonCoord l)

data LineDirection = Increasing
						| Decreasing
	deriving (Show)

coordValue :: Point -> WhichCoordinate -> Double
coordValue (Point x y) X = x
coordValue (Point x y) Y = y

getLineDirection :: Line -> LineDirection
getLineDirection line@(Line (Point sx sy) (Point ex ey)) = 
	if(startCompareValue < endCompareValue)
	then Increasing
	else Decreasing
		where 
			startPoint = Point sx sy
			endPoint = Point ex ey
			startCompareValue = coordValue startPoint (directionalCoord line) 
			endCompareValue = coordValue endPoint (directionalCoord line) 

data Angle = Angle {
	line :: Line
	, outsidePoint :: Point
	} deriving (Show)

outsidePointComparisonValue :: Angle -> Double
outsidePointComparisonValue (Angle line outsidePoint) = coordValue outsidePoint (comparisonCoord line)

outsidePointDirectionalValue :: Angle -> Double
outsidePointDirectionalValue (Angle line outsidePoint) = coordValue outsidePoint (directionalCoord line)

slope :: Line -> Double
slope (Line (Point sx sy) (Point ex ey))		= (ey - sy) / (ex - sx)  -- Infinity is a Double in Haskell!

yintercept :: Line -> Double
yintercept line@(Line (Point sx sy) _) 							= sy - (slope line) * sx

linePointToCompareWithOutsidePoint :: Angle -> Point
linePointToCompareWithOutsidePoint angle@(Angle line@(Line (startPoint) (Point ox oy) ) _) = 
	if coordToKeep == X
	then Point coordValueToKeep (m * coordValueToKeep + b) -- simple y=mx+b
	else Point (x startPoint) coordValueToKeep
		where 
			coordToKeep = directionalCoord line
			coordValueToKeep = outsidePointDirectionalValue angle
			m = slope line
			b = yintercept line

p = Angle (Line (Point (-2) 2) (Point (-3) 3)) (Point (-8) 1)
q = Angle (Line (Point 0 3) (Point 1 3)) (Point 2 5)
r = Angle (Line (Point 3 1) (Point 3 2)) (Point 8 1)
s = Angle (Line (Point 3 2) (Point 3 1)) (Point 8 1)
t = Angle (Line (Point (-3) 3) (Point (-2) 2)) (Point (-8) 1)
straightAngle = Angle (Line (Point 1 1) (Point 2 2)) (Point 3 3)


linePointComparisonValue :: Angle -> Double
linePointComparisonValue angle = coordValue (linePointToCompareWithOutsidePoint angle) (comparisonCoord (line angle))

opccVsLpcc :: Angle -> Ordering
opccVsLpcc angle = compare opValue lineValue
	where
		opValue = outsidePointComparisonValue angle
		lineValue = linePointComparisonValue angle

data AngleBreakdown = AngleBreakdown {
	comparing :: WhichCoordinate
	, lineDirection :: LineDirection
	, compareResult :: Ordering
} deriving (Show)

getAngleBreakdown :: Angle -> AngleBreakdown
getAngleBreakdown angle = 
	AngleBreakdown (comparisonCoord l) (getLineDirection l) (opccVsLpcc angle)
		where 
			l = line angle

getTurnDirectionFromBreakdown :: AngleBreakdown -> TurnDirection
getTurnDirectionFromBreakdown (AngleBreakdown _ _ EQ) = Straight
getTurnDirectionFromBreakdown (AngleBreakdown Y Increasing LT) = RightTurn
getTurnDirectionFromBreakdown (AngleBreakdown Y Increasing GT) = LeftTurn
getTurnDirectionFromBreakdown (AngleBreakdown Y Decreasing LT) = LeftTurn
getTurnDirectionFromBreakdown (AngleBreakdown Y Decreasing GT) = RightTurn
getTurnDirectionFromBreakdown (AngleBreakdown X Increasing LT) = LeftTurn
getTurnDirectionFromBreakdown (AngleBreakdown X Increasing GT) = RightTurn
getTurnDirectionFromBreakdown (AngleBreakdown X Decreasing LT) = RightTurn
getTurnDirectionFromBreakdown (AngleBreakdown X Decreasing GT) = LeftTurn

getTurnDirection :: Angle -> TurnDirection
getTurnDirection angle = getTurnDirectionFromBreakdown (getAngleBreakdown angle)

-- 11. Define a function that takes a list of 2D points and computes the direction of each successive triple. Given 
--  a list of points [a,b,c,d,e], it should begin by computing the turn made by [a,b,c], then the turn made 
--  by [b,c,d], then [c,d,e]. Your function should return a list of Direction.

data TestCase_ListOfPoints = TestCase_ListOfPoints {
	listOfPoints :: [Point]
	, expectedResults :: [TurnDirection]
}

testCase_ListOfPoints = 
    TestCase_ListOfPoints [(Point 1 1), (Point (-4) 2), (Point (-5) 4), (Point (-5) (-2)), 
        (Point (-3) (-3)), (Point (-2) 3), (Point 5 1), (Point 1 (-3)), (Point 1 (-2)), (Point 1 (-1)), (Point 1 (-2)),
        (Point 5 (-2)), (Point 3 (-2)), (Point 3 4)] 
        [RightTurn, LeftTurn, LeftTurn, LeftTurn, RightTurn, RightTurn, RightTurn, Straight, Straight, LeftTurn, Straight, RightTurn]

getTurnList :: [Point] -> [TurnDirection]
getTurnList [] = error "Must have at least 3 Points, dude"
getTurnList (x:[]) = getTurnList []
getTurnList (x:y:[]) = getTurnList []
getTurnList (x:y:z:[]) = [getTurnDirection (Angle (Line x y) z)]
getTurnList (x:y:z:xs) = getTurnDirection (Angle (Line x y) z) : getTurnList (y:z:xs)

-- Send this tweet: more haskell joy! hot damn!  book had silly exercise where math was harder than the haskell. NAILED it! Code would make guru puke but still!

-- Now in Chapter 4

-- My own playing around with break:

splitAtVowels :: [Char] -> ([Char], [Char])

splitAtVowels list = break isVowel list

isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel _ = False -- intentionally ifgnoring caps

-- Some other stuff not related to chapter 3 below this line
data a `Pair` b = a `Pair` b
                  deriving (Show)






