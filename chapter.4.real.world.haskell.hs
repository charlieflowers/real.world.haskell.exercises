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
					[]   -> (splitWith f rest)
					xs   -> [xs] ++ (splitWith f rest)
	where
		(prefix, suffix) = break f l
		rest = case suffix of
			[] 		-> []
			x:xs 	-> dropWhile f xs -- obliterate the "delimiters" from the output, just as "words" obliterates spaces
			




