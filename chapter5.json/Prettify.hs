-- Prettify.hs
module Prettify where

import SimpleJSON
import Numeric (showHex)

data Doc = Empty
            | Char Char
            | Text String
            | Line
            | Concat Doc Doc
            | Union Doc Doc
              deriving (Show, Eq)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty -- smart
text s = Text s

double :: Double -> Doc
double d = text (show d) -- remember, Doc is about rendering. Don't need the number, just the text representation of it.

line :: Doc -- Note the syntax for a parameterless function
line = Line

-- This appends one doc to another. I think (<>) must already be "known" to Haskell as an infix operator, and we are defining (or
--  possibly overriding) it here.
(<>) :: Doc -> Doc -> Doc
Empty <> b = b
a <> Empty = a
a <> b = a `Concat` b

-- The following functions need further study. I'm a bit in a hurry now.
hcat :: [Doc] -> Doc
hcat = fold (<>)

-- I gurss that the 1st parameter is the "step" function, and it takes a Doc as item and a Doc as accumulator? Close, come back and review.
fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

-- This op takes 2 docs and returns a third. 
(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

-- Every time we encounter a soft newline, we will maintain TWO ALTERNATE REALITIES. In one, the doc has been split to a new line. In the 
--   other, it has not. We do this using the Union constructor. They introduced Union because it means something different than Concat.
--   Union means, here;s an alternate reality for this part of the Doc. 

-- The flatten function replaces a Line with a space, and thus turns 2 separate lines into one line.
-- Come back and understand this deeper after yousee usage examples.
flatten :: Doc -> Doc 
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' ' 
flatten (x `Union` _) = flatten x
flatten other = other

-- This renders tersely and not friendly for human esyse
compact :: Doc -> String
compact x = transform [x]
   where transform [] = ""
         transform (d:ds) = 
            case d of 
               Empty            -> transform ds
               Char c           -> c: transform ds
               Text s           -> s ++ transform ds
               Line             -> '\n' : transform ds
               a `Concat` b     -> transform (a:b:ds)
               _ `Union`  b     -> transform (b:ds) -- AH, I see! For ugly rendering, IGNORE the flattened version, cuz ugly is ok.

-- This one is still a little unclear to me. obviously it has to do with putting commas (or whatever else) into a series. They
--   give no explanation and it appears to be using point free style so the specific calls to it don't tell me as much as i hoped.
punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p)  : punctuate p ds

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
   where best col (d:ds) =
               case d of
                  Empty -> best col ds
                  Char c -> c: best (col + 1) ds
                  Text s -> s ++ best (col + length ds) ds
                  Line -> '\n' : best 0 ds
                  a `Concat` b -> best col (a:b:ds)
                  a `Union` b -> nicest col (best col (a:ds))
                                            (best col (b:ds))
         best _ _ = ""
         nicest col a b | (width - least) `fits` a = a
                        | otherwise                = b
                        where least = min width col

-- This answers the question, "does it fit?"
fits :: Int -> String -> Bool
w `fits` _ | w < 0   = False
w `fits` ""          = True
w `fits` ('\n':_)    = True
w `fits` (c:cs)      = (w - 1) `fits` cs




         

