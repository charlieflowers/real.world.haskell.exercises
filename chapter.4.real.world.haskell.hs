-- Now in Chapter 4
import Data.List (isPrefixOf, isInfixOf)
import Data.Char (toUpper)
import Data.Char (digitToInt)

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

isLessThanFive :: Int -> Bool
isLessThanFive x = x < 5

-- Chapter 4 exercises, section one (after bit about string functions)

-- 1. Write your own “safe” definitions of the standard partial list functions, but make sure that yours never fail. As a hint, you might want to consider using the following types.
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just (tail xs)

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)

-- 2. Write a function splitWith that acts similarly to words, but takes a predicate and a list of any type, and splits its input 
-- 		list on every element for which the predicate returns False.

-- isSpace is for testing break to see how it compares to word.
isSpace :: Char -> Bool
isSpace ' ' = True
isSpace _ = False

splitWith :: (a -> Bool) -> [a] -> [[a]]

splitWith f [] = []
splitWith f l = case prefix of
					[]   -> meat
					xs   -> [xs] ++ meat
	where
		(prefix, suffix) = break f l
		rest = case suffix of
			[] 		-> []
			x:xs 	-> dropWhile f xs -- obliterate the "delimiters" from the output, just as "words" obliterates spaces
		meat = splitWith f rest

-- 3. Using the command framework from the section called “A simple command line framework”, write a program that prints the first word of each line of its input.

-- the command line framework from the book is saved in InteractWith.hs. I then copied it over to "firstWord.hs" and added my answer there.

-- 4. Write a program that transposes the text in a file. For instance, it should convert "hello\nworld\n" to "hw\neo\nlr\nll\nod\n".
-- I decided not to do this one, and here's why:
-- 
--  It is poorly specified. The only example they give has 2 lines. Should it handle an unlimited number of lines or not? Also, their example has all lines of SAME LENGTH.
--  Should it handle variable length or not? And if so, HOW should it handle variable length? There are a number of possible options. These requirements would dictate whether 
--- or not we could use zip, for example. 
--
--  Also, I can clearly see that I could do it, given any of the above requirements. I think their examples are not so well thought out.

myOdd :: [Int] -> [Int]

myOdd [] = []
myOdd (x:xs) = if odd x
				then x:myOdd xs
				else myOdd xs

foldl_test = foldl letterCount 0 ["Hi", "there"]

letterCount :: Int -> String -> Int
letterCount acc string = acc + (length string)

foldl_string = foldl helper "" "Hi there!!"
	where
		helper acc char = acc ++ [toUpper char]

rLetterCount :: String -> Int -> Int
rLetterCount string acc = acc + (length string)
		
foldr_test = foldr rLetterCount 0 ["Hi", "there"]

-- Exercises after foldr and foldl discussion:

-- 1. Use a fold (choosing the appropriate fold will make your code much simpler) to rewrite and improve upon the asInt function from the section called “Explicit recursion”. 
-- asInt_fold :: String -> Int
-- Your function should behave as follows. 13 comments
-- ghci> asInt_fold "101"
-- 101
-- ghci> asInt_fold "-31337"
-- -31337
-- ghci> asInt_fold "1798"
-- 1798

--Extend your function to handle the following kinds of exceptional conditions by calling error. 3 comments
--
-- ghci> asInt_fold ""
-- 0
-- ghci> asInt_fold "-"
-- 0
-- ghci> asInt_fold "-3"
-- -3
-- ghci> asInt_fold "2.7"
-- *** Exception: Char.digitToInt: not a digit '.'
-- ghci> asInt_fold "314159265358979323846"
-- 564616105916946374

asInt_fold :: String -> Int

-- The function as follows works but does not handle edge cases.
--asInt_fold string = fst (foldr helper (0,0) string)
--	where
--		helper char (sum,place) = (newValue, newPlace)
--			where 
--				newValue = (10 ^ place) * (digitToInt char) + sum
--				newPlace = place + 1

-- Got the above part pretty quick. It's the gist. You do foldR, "r" being key, because then you know each successive call is for the next place value.
--  You need to track the place value and the previous sum, so the accumulator has to be a pair.
--  Therefore, foldr itself will return a pair, since it always returns the final accumulator value. So, use fst to pull the final desired value out.
--  Really, I think the only significant improvements (with the lang features I know so far) is aesthetics.

-- Now, let's deal with the edge cases....

asInt_fold "" = error "You can't be giving me an empty string now"
asInt_fold "-" = error "I need a little more than just a fucking dash, dude"
asInt_fold string | isInfixOf "." string = error "I can't handle decimal points"
asInt_fold ('-':xs) = -1 * (asInt_fold xs) -- this should handle minus sign, which is only valid at the head (ignoring whitespace for now)
asInt_fold string = fst (foldr helper (0,0) string)
	where
		helper char (sum,place)    | place == 9 && digitValue > 2          = throwMaxIntError
		                           | maxInt - sum < newPlaceComponent      = throwMaxIntError
                                   | otherwise                             = (newValue, newPlace)
			where
				digitValue =  (digitToInt char)
				placeMultiplier = (10 ^ place)
				newPlaceComponent = placeMultiplier * digitValue
				newValue = newPlaceComponent + sum
				newPlace = place + 1
				maxInt = 2147483647
				throwMaxIntError = error "The value you passed is larger than maxInt, which is 2147483647"

-- hmmm, the last edge case is interesting. Something overflowed. It (on my machine) can handle up to 9 places before it all goes to shit.
-- GOT IT!!! Int is either 32-bit or 64-bit depending on machine (or more). On my machine, I just confirmed it is 32 bit.
--  Now, 2^32 = 4,294,967,296, but I can't go that high! Why? Because THE FIRST BIT IS USED FOR THE SIGN. Therefore, I have 31 bits to work with. Thus,
--  the max value an Int can hold on my machine is 2^31 - 1, which is 2147483647. And here's proof straight from interpreter:
-- *Main Data.Char Data.List> asInt_fold "2147483648"
-- -2147483648 -- this is incorrectly negative because we overflowed over into the sign bit
-- *Main Data.Char Data.List> asInt_fold "2147483647"
-- 2147483647 -- this is correct.

-- How to handle that edge case? Let's put a case expression around the definition of newPlace, so it will error out if things get too high. I did that, but 
--  I don't like it. Would be more readable at a higher level, so I will make that change once I get it working.

-- My first attempt failed. This value, which is too high, does not error...
-- *Main Data.Char Data.List> asInt_fold "2147483648"
-- -2147483648

-- OK, now, got it. With the following interesting notes: 
-- 1. At first, I had 2 equations for defining helper (each with different patterns), but I wanted both to share the same where or let clause. I don't think there's
--  a way to do this though. So I used just one equation, with just one pattern, and guards. That way, they could share the same where block. Find out if I'm right 
--  that multiple equations can't share the same where / let.
--
-- 2. I made the style choice that, anytime I was repeating even a little snippet of a formula, I made that into a variable that I referred to twice. This is 
--  nothing but DRY. Is this considered good practice in Haskell, or not, and why?




