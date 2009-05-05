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






