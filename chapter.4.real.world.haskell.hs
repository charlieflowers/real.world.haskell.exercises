-- Now in Chapter 4
import Data.List (isPrefixOf, isInfixOf, foldl')
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

-- HEY!! I like my solution and all that, and it led to me asking 2 good StackOverflow questions. HOWEVER, after all that, it turns out that FOLDL was a MUCH BETTER 
--  WAY TO GO!!! Why? Well, because all you woulda needed to do was multiply the incoming sum by ten! No need to keep track of the place. Therefore, the accumulator
--  could be nothing more than just a single value (doesn't have to be a tuple anymore!!)

-- NOW, i just LEARNED A GREAT THING ON STACK OVERFLOW!! It is a HASKELL IDIOM: DEFINE A FUNCTION IN *ITS OWN WHERE CLAUSE*!!! This lets MULTIPLE EQUATIONS SHARE 
--  THE SAME WHERE CLAUSE ... because it will be THE SAME WHERE CLAUSE THAT THE VARIABLES ARE IN!!
-- I am going to write my asInt_fold that way:

someFunction v1 v2 = f 
    where
        f 99 | difference < 4 = 0
        f v3 = difference ^ v3
        difference = v1 - v2


-- INTERESTING NOTE: I actually had the following function correct, but it was giving me "place not in scope" and "theChar not in scope" in the where clause. Why?
--  BECAUSE THE INDENTATION WAS WRONG! Two learnings from this:
--  1. Don't mix spaces and tabs. It could end up looking right to eyeballs, but not being at all what you meant to compiler
--  2. When you get surprising scope errors, try using the syntax that makes whitespace insigificcant. If that works, then clean up your whitespace!

asInt_idiom_fold :: String -> Int

asInt_idiom_fold "" = error "You can't be giving me an empty string now"
asInt_idiom_fold "-" = error "I need a little more than just a dash, dude"
asInt_idiom_fold string | isInfixOf "." string = error "I can't handle decimal points"
asInt_idiom_fold ('-':xs) = -1 * (asInt_idiom_fold xs) 
asInt_idiom_fold string = fst (foldr helper (0,0) string)
  where
    helper theChar (sum,place) = f
     where
                f | place == 9 && digitValue > 2                = throwMaxIntError
                  | maxInt - sum < newPlaceComponent            = throwMaxIntError
                  | otherwise                                   = (newValue, newPlace)
                digitValue =  digitToInt theChar
                placeMultiplier = (10 ^ place)
                newPlaceComponent = placeMultiplier * digitValue
                newValue = newPlaceComponent + sum
                newPlace = place + 1
                maxInt = 2147483647
                throwMaxIntError = error "The value is larger than max of 2147483647"

-- 2. The asInt_fold function uses error, so its callers cannot handle errors. Rewrite it to fix this problem.
-- type ErrorMessage = String
-- asInt_either :: String -> Either ErrorMessage Int
--
-- ghci> asInt_either "33"
-- Right 33
-- ghci> asInt_either "foo"
-- Left "non-digit 'o'"
-- 
-- First, some notes:
--  A. I'm gonna write it using foldl', which would've been the better choice anyway (as the book hinted) [DON'T FORGET TO USE PRIME!!! FOLDL', NOT FOLDL]
--  B. Remember, "type" creates an "alias", merely a synonym for the other type (but not interchangable). Sort of like C++ typedef.
--  C. The type Either has 2 value ctors: Left a | Right a. Per rqmts, use Right for ok, Left for error.
--

type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int

-- the following first try fails: "err" is not in scope (I'm trying to be DRY about my error handling). So, next I'll try to put the function into where clause as mentioned before.
-- asInt_either ""                             = err "Blank string given"
-- asInt_either "-"                            = err "Need more than dash"
-- asInt_either string | isInfixOf "." string  = err "Can't handle decimal points"
-- asInt_either ('-':string)                   = negate (asInt_either string)
-- asInt_either string                         = foldl' step 0 string
--    where 
--      step sum char = Right (sum * 10 + (digitToInt char))
--      err text = Left text

asInt_either ('-':string) = case asInt_either string of
   Right n -> Right (negate n)
   other   -> other

asInt_either string = f
   where 
      f = case string of
         ""                   -> err "Blank string given"
         "-"                  -> err "Need more than dash"
         s | isInfixOf "." s  -> err "Can't handle decimal points"
         _                    -> foldl' step (Right 0) string
      err text = Left text
      maxInt = 2147483647
      maxIntDivBy10 = 214748364
      step (Right sum) char = if hasOverflow
                      then err "The number is larger than max Int of 2147483647"
                      else Right (sum * 10 + digitValue)
         where
           digitValue = (digitToInt char)
           hasOverflow = if sum > maxIntDivBy10 then True
                         else if digitValue > maxInt - (sum * 10) then True
                         else False
      step other _ = other




