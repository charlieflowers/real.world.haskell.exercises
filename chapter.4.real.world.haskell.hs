-- Now in Chapter 4
import Data.List (isPrefixOf, isInfixOf, foldl')
import Data.Char (toUpper)
import Data.Char (isUpper)
import Data.Char (digitToInt)
import GHC.Unicode (Char.isSpace)

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

charlieMaxInt = 2 ^ 31 - 1
charlieMaxIntAsString = "2147483647"
charlieTooBigIntAsString = "2147483648"

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
      step (Right sum) char = if nonDigit char then err ("non-digit '" ++ [char] ++ "'")
                      else if hasOverflow
                      then err "The number is larger than max Int of 2147483647"
                      else Right (sum * 10 + digitValue)
         where
           digitValue = (digitToInt char)
           hasOverflow = if sum > maxIntDivBy10 then True
                         else if digitValue > maxInt - (sum * 10) then True
                         else False
           nonDigit char = char < '0' || char > '9'
      step other _ = other

-- 3. The Prelude function concat concatenates a list of lists into a single list, and has the following type.
	-- file: ch04/ch04.exercises.hs
	-- concat :: [[a]] -> [a]
--  Write your own definition of concat using foldr.

-- This was my first attempt. It was correct (had to fight thru some errors). However it is LAME. You'll know why when you see the BEST solution.
myLameCat :: [[a]] -> [a]
myLameCat input = foldr step [] input
   where 
      step acc item = acc ++ item

-- Duh! HERE is the BETTER solution!!
myCat :: [[a]] -> [a]
myCat input = foldr (++) [] input

-- Now, let's analyze WHY I didn't think of that, and HOW I CAN think of that!
-- Mainly, I just started with the mindset that there must be a step function! That is a wrong assumption. Start by looking for a function to plug in there, 
--  and only if you can't find one should you resort to coding one and using the name "step".
--  REMEMBER to think in terms of "transforming the list", replacing cons with some operation, and replacing [] with something!!!

-- 4. Write your own definition of the standard takeWhile function, first using explicit recursion, then foldr. 

recursion_takeWhile :: (a -> Bool) -> [a] -> [a]

recursion_takeWhile p [] = []
recursion_takeWhile p (x:xs) = if p x
                               then x: (recursion_takeWhile p xs)
                               else []

-- Interesting that foldr goes backwards thru list, but takeWhile takes the front of the list. At first I thought this was a problem. But actually,
--  I think it is helpful. If p ever fails, wipe out acc. Otherwise, keep adding to it.
ftw_foldr_takeWhile :: (a -> Bool) -> [a] -> [a]

ftw_foldr_takeWhile p list = foldr step [] list
  where 
    step item acc = if p item 
                    then item : acc
                    else []

-- 5. The Data.List module defines a function, groupBy, which has the following type. No comments
-- 
-- -- file: ch04/ch04.exercises.hs
-- groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
-- Use ghci to load the Data.List module and figure out what groupBy does, then write your own implementation using a fold. 
-- 

-- Some thoughts:
-- They said use a fold, but not which one. That's up to me (first guess, foldl').
-- The idea is that the function you provide is an "equality test", but it can be anything that meets the type signature.

-- Some experimental / test stuff for interpreter
patternTest = case [[3,3],[4,4]] of 
	((x:xs):ys) -> (2:x:xs):ys 
	otherwise -> error "no match"

listTest = case [3] of
   (x:xs) -> "Was matched by (x:xs)"
   _      -> "Was NOT matched by anything!"

-- END experimental

gb_groupBy :: (a -> a -> Bool) -> [a] -> [[a]]

-- I'm going to try very hard to have the accumulator be nothing but the final result. I think I can do that with 2 levels of pattern matching.
gb_groupBy p input = foldr step [] input
   where
      step item acc = case acc of
           []                           -> [[item]]
           ((x:xs):ys)                  -> if p x item
                                           then (item:x:xs):ys
                                           else [item]:acc
           []:_                         -> error "This pattern should never happen"

-- Next question: does -fwarn-incomplete-patterns show that I've covered all pattern possibilities? Answer: it does now, after I got advice on StackOverflow.

-- 6. How many of the following Prelude functions can you rewrite using list folds?
-- 
-- any 
-- cycle 
-- words 
-- unlines 
--
-- For those functions where you can use either foldl' or foldr, which is more appropriate in each case? 10

-- Notes:
-- 1. DON'T FORGET TO ANSWER THEIR QUESTION FOR EACH: IS FOLDL' OR FOLDR BETTER??????
-- 2. Note that the way they phrased the question seems to indicate that it MAY BE IMPOSSIBLE to do some of them with folds.
-- 3. ALSO NOTE that they didn't actually SAY to implement them! They're asking about possibility! I may try to write them all anyway, but not if it takes ALL NIGHT!! :)

myAny_thatFailsWithInfiniteList :: (a -> Bool) -> [a] -> Bool
myAny_thatFailsWithInfiniteList p list = foldl' step False list
   where 
      step True item = True
      step False item = p item

-- Yes, you can implement it with a fold. Seems foldl' and foldr are on equal footing here.
--
-- AHH BUT WAIT!!! Consider an infinite list handed to you. You have to remember, LAZY EVALUATION!! It may actually "figure out" that it never has to hit
--   the list again. And if it doesn't "figure that out" based on the way you wrote it so far, then you can definitely re-write it to do so.
-- It FAILS RIGHT NOW, on infinite list. So, let's take another stab....

-- Let's just "transliterate" it to a foldr and see what we get.
myAny_foldr_alsoNoInfiniteList :: (a -> Bool) -> [a] -> Bool
myAny_foldr_alsoNoInfiniteList p list = foldr step False list
   where
      step _ True     = True
      step item False = p item

-- Nope. In fact, that one gets a stack overflow on infinte list (first one probably does too, i just didn't let it run)

-- So, how WOULD you write it to handle infinite lists? I'm really not sure, since both folds set out to walk the entire list.

-- Based on book comments, I think if I call (pred a || b), it might work. Not sure why yet, but let's see it work and then dissect it.
myAny :: (a -> Bool) -> [a] -> Bool
myAny p list = foldr step False list
   where
      step item acc = p item || acc

-- YES!!! That DOES work on an infinite list!!! Now, the question is WHY??
--  (and from there launched a very intensive learning period! Read "A tutorial on the universality and expressiveness of fold", 
--  finished "Why FP matters", and got some great answers to questions I asked on StackOverflow. I think FP appears to be very 
--  powerful indeed, and to get even more out of it, you can become a mathematician ... which is cool with me! Helluva crossover 
--  between 2 things I'm interested it!)

-- And during all this, the following fundamental principle came to me (which I of course twittered the moment it hit): 
-- "If you want more software power, you'll need more abstraction. Then, you're limited to humans who can mentally handle those abstractions. That
--  is a fundamental law of the universe, and it is why a few top notch developers will always outpace many intermediate developers."

-- And I now KNOW why that myAny impl works on infinite lists! It is because the || operator is strict in its first arg, but not its second arg. Also,
--  the way I've written it amounts to a call to step that is of the form (True || acc). Anytime you have (True || anything whatsoever), it will 
--  return True without ever evaluating "anything whatsoever".

-- Also, based on this, I hypothesize that myAny will NEVER RETURN if I give it an infinite list that does NOT have an element that meets the predicate. In 
--  other words, EVEN THIS IMPL OF MYANY DOES NOT WORK ON **ALL** INFINITE LISTS!! Should get stack overflow if i let it go that far.
--  Testing now on (myAny odd [2,4 ..])
--  Well, it is STILL RUNNING, and this is very interesting. Perhaps I WON'T get a StackOverflow, due to TAIL CALL OPTIMIZATION! Maybe it will just 
--   run forever. It does not have to build up a very large thunk at all! Either way, of course, it can never return an answer.

-- Continuing with the Real World Haskell exercises:
-- implementing cycle with fold.

myCycle :: [a] -> [a]

-- From playing in interpreter, I gather that this returns one list, and that the list is infinite, and all it does is repeat the initial list over and over.
--  Yep, just confirmed it with documentation.
--  I'm going to start from the mindset that foldr is probably best for this too, simply because it can handle infinite lists. foldl and foldl' never can as far as I can see.
-- myCycle input = foldr (:) input input
-- That one doesn't work! It ONLY repeats the list ONE TIME, and then ends. So myCycle [1,2,3] gives [1,2,3,1,2,3]
myCycle_foldrAndRecursion input = foldr (:) (myCycle input) input -- Is that cheating? I'm using explicit recursion AND foldr.
-- Perhaps that IS cheating, but it definitely WORKS. And when preceded with take, as in (take 500 (myCycle [1,2,3])), it only gives 500 elements! No problemo.

-- Can it be done without the explicit recursion? Surely it can, eh? Let's see....
myCycle input = foldr (:) (foldr (:) input input) input -- now THAT is TRULY some WEIRD LOOKIN' SHIT!
-- Well, THAT'S STRANGE!!!! It ONLY REPEATS THE LIST 3 TIMES, NO MATTER WHAT!!??!!
-- Oh, I get it! If I want 4 repetitions, I must do this:
myCycle4reps input = foldr (:) (foldr (:)    (  foldr (:) input input   )          input) input 

-- Suppose, just for the hell of it, I make the accumulator a pair, one side of which is the whole list.
myCyclePair input = foldr step (input, []) input
   where 
      step item (input, acc) = (input, item : acc)
-- No, that doesn't work either. It merely duplicates the list one time.

-- Seems that the only way to make the list repeat infinitely is to do some operation that is infinite. Recursion is one such operation. I need to repeat the fold forever.
--  Not sure if there is any other way to do it. If not, that means you have to use foldr and explicit recursion together.

-- Here's a cheat. What if I use specification by example to get the job done, and just carry the foldr around like baggage because the exercise requires it!
myCyclebaggage input = foldr (:) [] [repeat input]
-- Yep, that works. But it too is kind of a cheat, yes?

-- WAIT A MINUTE!! What if I turn the input into a list of lists! Then, the first item i get is the whole list. Still, that would be the one item in my list, and it 
--  would be the only time my step function gets called. I'd have to repeat it.

-- You have TWO CHOICES. Either 
-- 1) your step function gets called only once, and you do the infinite magic there, OR
-- 2) you magically infinitize the original input that you feed to foldr, so that your step function gets called an infinite number of times.

-- One way to do # 1 above would be to MAKE STEP ITSELF RECURSIVE!!

myCyclestepRecursive input = foldr step [] [input]
   where
      step item acc = item ++ (step item [])
-- YES, THAT'S RIGHT!!!!! But it still uses explicit recursion, so perhaps it is still a little bit cheating.

-- What if step calls foldr again? Maybe that is the right solution. Let's see.
myCyclefoldrTofoldr input = foldr step [] [input]
   where
      step item acc = item ++ (foldr step [] [input])

-- YES!! I like that solution VERY MUCH!! No explicit recursion whatsoever, just TWO "nested" (if you will) usages of foldr!!

-- Let's see if I can do it without the ugly ++
myCycleNoExplicitRecursion [] = error "empty list"
myCycleNoExplicitRecursion input = foldr step [] [input]
   where
      step [] _ = foldr step [] [input]
      step (x:xs) acc = x: (step xs acc)

-- YES!!! That is it! That is my preferred solution!!!!

-- Now, I gotta do one with foldl', just to see if I learn anything from it.

myCycleleft input = foldl' step [] [input] 
   where
      step _ [] = foldl' step [] [input]
      step acc (x:xs) = x:(step acc xs)

-- Now, for "words"

charIsSpace = GHC.Unicode.isSpace

testCharIsSpace = GHC.Unicode.isSpace ' '
secondTest = charIsSpace ' '

myWords_FailsOnInfiniteList :: String -> [String]
myWords_FailsOnInfiniteList string = foldr step [] (dropWhile charIsSpace string)
   where 
      step space ([]:xs)      | charIsSpace space = []:xs    
      step space (x:xs)       | charIsSpace space = []:x:xs
      step space []           | charIsSpace space = []
      step char (x:xs)                            = (char : x) : xs
      step char []                                = [[char]] 

myWords_catInsteadOfCons :: String -> [String]
myWords_catInsteadOfCons string = foldr step [] (dropWhile charIsSpace string)
   where 
      step space ("":xs)      | charIsSpace space = [""] ++ xs -- keep it exactly the same as it was!    
      step space (x:xs)       | charIsSpace space = [""] ++ [x] ++ xs
      step space []           | charIsSpace space = []
      step char (x:xs)                            = [char : x] ++ xs
      step char []                                = [[char]] 

-- Using cat instead of cons doesn't change anything. The function works on finite, but stack overflows on infinite.

-- PICK UP RIGHT HERE NEXT TIME!! MAKE MYWORDS THAT DOES DEAL WITH INFINITE LISTS!! PUT THAT ISSPACE CHECK ON THE FRONT OF AN || SO YOU CAN SHORT-CIRCUIT!

{-
myWords :: String -> [String]
myWords string = foldr step [] (dropWhile charIsSpace string)
   where
-}

-- Let's do some experimenting here. Can we match an infinite list against x:xs, as long as we don't eval xs? YES IT IS!!
-- YES, IT IS!

-- Can you even recurse using xs (which, btw, mean passing xs as a funciton arg, which we KNOW will be a thunk?) YES, YOU CAN!!
-- YES, YOU CAN!!

-- test (x:xs) = x:test xs
    
-- Well, it seems like that right there is your answer! How and why am I adding the entire xs to the end of my stuff anyway?? Is that even RIGHT?
--  What I'm splitting up there is the ACCUMULATOR! NOT the whole list. And YES, that makes total sense. 

-- Hey, it just occurred to me that the dropWhile may actually be what is causing it to eval the whole list. Let's take it off just to see what we get. Actually, do I 
--  need it anyway? After all, my step definition ALREADY HANDLES SPACES!!
--  Well, turns out I DO need it anyway, or some other mechanism, because otherwise a string starting with spaces returns result starting with "".
--  And ... no, removing the dropWhile DOES NOT make it work on infinite lists!
myWords_withoutDropWhile :: String -> [String]
myWords_withoutDropWhile string = foldr step [] string
   where 
      step space ([]:xs)      | charIsSpace space = []:xs    
      step space (x:xs)       | charIsSpace space = []:x:xs
      step space []           | charIsSpace space = []
      step char (x:xs)                            = (char : x) : xs
      step char []                                = [[char]] 

-- Even though I can't yet pinpoint why these functions don't work against infinite lists, I do know that using break is a perfectly legit approach to the exercise. And 
--  I know that IF YOU ONLY DIRECTLY ACCESS THE FIRST TUPLE OF BREAK'S RESULT, you will be ok with an infinite list. Therefore, let me do that.
{-
BUT I AM HAVING A HARD TIME WRITING THIS!! LET ME FIRST WRITE ONE USING EXPLICIT RECURSION! THEN, we'll see if I can figure out how to write one using fold. 
myWords_break :: String -> [String]
myWords_break input = foldr step [] input
   where
      step char 
      nextWord remainingText = break isSpace remainingText
      nextWord []            = []
-}

prepareInput input = dropWhile charIsSpace input

myWords_explicitRecursion_break :: String -> [String]
myWords_explicitRecursion_break input = case break charIsSpace (prepareInput input) of
   ("", "")   -> [] 
   ("", rest) -> myWords_explicitRecursion_break rest
   (fw, rest) -> fw:myWords_explicitRecursion_break rest

-- OK, my main problem before was in NOT KNOWING HOW BREAK ITSELF WORKS!! There are cases where it returns ("",""). Also, it alone would NEVER CONSUME ANY SPACES, leading to 
--  infinite loops that get you nowhere.

-- Does my explicit recursion version handle infinite lists? My guess is YES, because the "rest" is only passed as part of function calls, and therefore will be a thunk.
-- And INDEED, the answer is yes, as proven by this call: take 15 (myWords_explicitRecursion_break (cycle "fuck "))

-- NOW, let's see about turning my explicit recursion version into a fold!!

-- I think it is time to punt on this. They haven't taught this yet, and I've spent a lot fo time and effort trying to figure it out. That will pay off when they 
--  actually DO teach it. I think the smartest, most pragmatic course of action is to see what other answers have been submitted by other readers, see if any of 
--  them work vs infinite lists, and if so, see if i can determine why.

myWords_davidb lst = let (word, rest) = foldr step ("", []) lst
                       in if null word then rest else word : rest
  where
    step c (word,rest)
      | charIsSpace c = if null word then ("",rest) else ("", word:rest)
      | otherwise = ((c:word), rest)

-- davidB does NOT work on infinite lists!

myWords_giorgio :: String -> [String]
myWords_giorgio xs = foldr step [""] xs
   where 
      step x result | not . charIsSpace $ x    = [x:(head result)]++tail result
                    | otherwise            = []:result

-- YES!!!  Giorgio's version DOES work against infinite lists!! Let's break it down!
-- Well, there's nothing TO break down really. It looks very similar to mine above, but it works against infinite and mine does not!

-- ***********************************************************
-- OK!! GOT SOME MORE ANSWERS FROM STACKOVERFLOW.COM, AND NOW I BELIEVE I KNOW HOW TO MAKE MYWORDS WORK ON INFINITE LISTS!! 
--  The KEY is to produce SOME PART of the output BEFORE you eval the second arg to step!!

myWords_worksOnInfiniteButIWantToTweak :: String -> [String]
myWords_worksOnInfiniteButIWantToTweak string = foldr step [""] (dropWhile charIsSpace string)
   where 
      step space acc | charIsSpace space     = "":acc -- this assures there is ALWAYS a string at the head that we want to add to
      step char acc                          = [char:(head acc)] ++ tail acc

-- Let's see if I can get rid of head and tail via pattern matching
--  YES, YOU CAN!!!! Works just FINE! 
myWords2 :: String -> [String]
myWords2 string = foldr step [""] (dropWhile charIsSpace string)
   where 
      step space acc | charIsSpace space     = "":acc -- this assures there is ALWAYS a string at the head that we want to add to
      step char (x:xs)                       = [char:x] ++ xs

-- Now, let's see if we can get rid of (++)
myWords3 :: String -> [String]
myWords3 string = foldr step [""] (dropWhile charIsSpace string)
   where 
      step space acc | charIsSpace space     = "":acc -- this assures there is ALWAYS a string at the head that we want to add to
      step char (x:xs)                       = (char:x):xs









-- HEY! Here's a cool POINT FREE STYLE definition for a function to test myWords against an infinite list!
testMyWords f = take 5 . (f . cycle)







-- Next, let's do "unlines" as a fold.

myUnlines :: [String] -> String
myUnlines list = foldr step "" list
   where 
      step word ""  = word
      step word acc = word ++ "\n" ++ acc

myCapCount :: String -> Int
myCapCount = length . filter (isUpper . head) . words








   












