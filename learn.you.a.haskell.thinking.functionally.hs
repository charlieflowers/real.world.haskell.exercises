import Data.List

-- Some problems from "Learn You a Haskell", the "thinking functionally" chapter.

doCalc :: String -> Double
doCalc "" = 0
doCalc s  = processTerm [] (words s)
   where
      processTerm [x] [] = x
      processTerm (x:y:xs) ("+":terms) = processTerm ((y + x):xs) terms
      processTerm (x:y:xs) ("-":terms) = processTerm ((y - x):xs) terms
      processTerm (x:y:xs) ("*":terms) = processTerm ((y * x):xs) terms
      processTerm xs (n:terms)         = processTerm ((read n :: Double):xs) terms

-- OK!!! Not bad. This gets the correct answer. It obviously needs several improvements, but the good news is it did not take me long to figure it out.
-- Here's what needs to be improved: 
-- 1. The duplication. Somehow, turn the string for the operator ("+", "-" or "*") into the ACTUAL operator function to be called.
-- 2. This could be done with a fold. I should do that just for practice if nothing else.
-- 3. I highly doubt it is exhaustive in terms of pattern matching. Not cool.
-- 4. No good error handling for unexpected input (unknown ops, failed parsing to number, not well-formed so stack has multiple items, etc.)
-- 5. I'm sure it can be converted to point free style. I tried, but had to punt in several different ways.

-- So, let's read on and see what the WRITER has in store for us!

-- Cool, the first thing he does is use the Num typeclass as the return type. I should know about these because he covered them. Need to start being aware of which ones exist
--   and when to use them.

solveRPN :: (Num a) => String -> a
solveRPN s = error "Not implemented"

-- He reversed the stack, just as I did, so we can add new items to the front of the stack.
-- His stuff looks ALMOST EXACTLY LIKE MINE! In fact, he starts with the SAME DUPLICATION.
-- Since he is using the Num typeclass instead of Double, he does not have to put a type annotation on the "read" call.
-- AHH! But he DOES need to call "read" on the string and GET a value of type "a". Therefore, "a" must ALSO be in the "Read" typeclass!

-- COOL! He does not take it much farther than that!!! He says we'll be more fault tolerant when we MASTER MONADS (VERY SHORTLY!!)

-- SO ... here's my impl with foldl, done without looking at his impl (I just read his, but now I am writing this based on my understanding rather than copying his)

solveRPNWithFold :: String -> Float

solveRPNWithFold = head . foldl' helper [] . words
   where
      -- helper word stack
      helper (x:y:xs) "+" = (y + x):xs
      helper (x:y:xs) "-" = (y - x):xs
      helper (x:y:xs) "*" = (y * x):xs
      helper stack n      = (read n):stack

-- Excellent, I did this correctly, the only thing I got wrong at first was the order of the args to the folding function. accumulator first, then the item.