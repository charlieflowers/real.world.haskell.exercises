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

-- This takes FIRST a single Doc which contains the desired "puncutation". Seen it called with a single-char Doc containing ',', for ex.
-- SECOND, takes a list of Docs to apply punctuation to. For ex, seen it called with a list of rendered object fields.
punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = [] -- empty list returns empty list
punctuate p [d]    = [d] -- single item list ... just return the same single item list
punctuate p (d:ds) = (d <> p)  : punctuate p ds -- A multi item list: add the doc to the punctuation doc, then recurse on the tail.

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

-- Exercise 1: fill
-- Here's how I interpret their incomplete, ambiguous requirements: 
-- Completely ignore soft line breaks. Those are merely there for actual *printing*, not rendering. They count as a space, not 
--  as a line break. Therefore, only consider the LEFTHAND SIDE of unions.
-- Find each line's hard line break. If it is < the desired width, then prepend the necessary spaces. If not leave it alone.
-- Do this to every line in the document.

-- This takes a Doc, and returns a list of Pairs. Each Pair contains the length of the line and the Doc for the Line ending.
--  NOTE that it IGNORES soft breaks, aka (Union (Char ' ') Line)
--  hardLines :: Doc -> [(Int, Doc)]
--  hardLines doc = getLine 0 [doc]
--     where getLine startCol (d:ds) = 
--              case d of
--                 x `Union` y -> getLine startCol [x]
--                 Empty -> getLine startCol ds
--                 Char c -> getLine (startCol + 1) ds
--                 Text s -> getLine (startCol + length s) ds
--                 Line -> (startCol, d):getLine 0 ds
--                 a `Concat` b -> getLine startCol (a:b:ds)
--           getLine startCol [] = [(startCol, Empty)]

manyLinesDoc :: Doc
manyLinesDoc = text "Hello " <> text "you" <> line <> text "crazy" <> text " world"

hardLines :: Doc -> [(Int, Doc)]
hardLines doc = getLine 0 [] [doc]
   where getLine startCol acc (d:ds) = 
            case d of
               x `Union` y -> getLine startCol acc [x]
               Empty -> getLine startCol acc ds
               Char c -> getLine (startCol + 1) acc ds
               Text s -> getLine (startCol + length s) acc ds
               Line -> acc ++ ((startCol, d):getLine 0 acc ds)
               a `Concat` b -> getLine startCol acc (a:b:ds)
         getLine startCol acc [] = acc ++ [(startCol, Empty)]

-- It's cool that I was able to make that hardLines function. But really, it's not what we need for fill. The thing bugging me is 
--  I sense the opportunity for reuse, but haven't yet quite clicked onto the right way to acheive it. So I'll make fill, and it 
--  will look a lot like "pretty" and "best" and "hardLines", but without reusing anything. Then, between my own progress and the 
--  book, it will come to me.

-- Basically, you're going to walk through the doc, and for each node, you're going to return a node yourself. Most times, the node
--  you return will be the same node you got. But sometimes, the node you return will be a different node (because that's the transform
--  you're doing). Actually, sounds like a fold to me. 

-- This function will loop thru the doc you pass, and for each doc node, it will call a transform function, giving that function the 
--  starting col position and the Doc. the transform function should then return the new Doc that it wants to replace that Doc with.
-- transformDoc :: Doc -> (Int -> Doc -> Doc) -> Doc
-- transformDoc doc transformFunction = helper 0 [doc]
-- 
-- NO, I am punting on that for now too. Here's plain ol fill, with no provisions for reuse.
fill :: Int -> Doc -> Doc
fill desiredWidth doc = processNode 0 [doc]
	where 
		processNode col (d:ds) = 
			case d of 
				Empty -> Empty <> processNode col ds
				Char c -> Char c <> processNode (col + 1) ds
				Text s -> Text s <> processNode (col + length s) ds
				Line -> spaceOut col <> Line <> processNode 0 ds
				a `Concat` b -> processNode col (a:b:ds)
				-- need to translate the left side. the right side is already shortened artificially
				x `Union` y -> processNode col (x:ds) `Union` processNode col (y:ds)
		processNode col [] = spaceOut col
		spaceOut col = Text (replicate (desiredWidth - col) ' ')

-- This is from another reader. I don't like it because I don't think it meets my interpretation of the requirements. In particular, I 
--  don't think it will fill out every line. Let me test it and see.
otherFill :: Int -> Doc -> Doc
otherFill width x = x <> text (replicate len ' ')
   where len = width - (length (compact x))
-- Yes, testing confirms this. This is a cop out of the exercise.

-- NESTING AND INDENTATION. This is here to fulfill exercise 2, which reads as follows: 
-- 2. Our pretty printer does not take nesting into account. Whenever we open parentheses, braces, or brackets, any lines that 
--  follow should be indented so that they are aligned with the opening character until a matching closing character is encountered.
--  Add support for nesting, with a controllable amount of indentation. 
--  nest :: Int -> Doc -> Doc

nest :: Int -> Doc -> Doc
nest indentLevel doc = processNode 0 [0] [doc]
	where 
		processNode col nestStack (d:ds) = 
			case d of 
				Empty -> Empty <> processNode col nestStack ds
				Char c -> Char c <> processNode (col + 1) newNestStack ds
					where newNestStack = case c of 
						c | c == '{' || c == '[' -> (col + 1 + indentLevel):nestStack
						c | c == '}' || c == ']' -> tail nestStack
						otherwise                -> nestStack
				Text s -> Text s <> processNode (col + length s) nestStack ds
				Line -> Line <> indent (head nestStack) <> processNode 0 nestStack ds
				a `Concat` b -> processNode col nestStack (a:b:ds)
				x `Union` y -> processNode col nestStack (x:ds) `Union` processNode col nestStack (y:ds)
		processNode col nestStack [] = Empty
		indent numberOfSpaces = Text (replicate numberOfSpaces ' ')

nest2 :: Int -> Doc -> Doc
nest2 indentLevel doc = processNode 0 [(0,True)] [doc]
	where 
		processNode col nestStack (d:ds) = 
			case d of 
				Empty -> Empty <> processNode col nestStack ds
				Char c -> case c of 
					c | c == '{' || c == '[' -> indent col nestStack <> Char c <> processNode (col + 1) (((col + 1 + indentLevel),False):nestStack) ds
					c | c == '}' || c == ']' -> indent col nestStack <> Char c <> processNode (col + 1) (tail nestStack) ds
					otherwise -> indent col nestStack <> Char c <> processNode (col + 1) nestStack ds
				Text s -> indent col nestStack <> Text s <> processNode (col + length s) nestStack ds
				Line -> Line <> processNode 0 newNestStack ds
					where newNestStack = case nestStack of
						((n,False):ds) -> (n,True):ds
						_              -> nestStack
				a `Concat` b -> processNode col nestStack (a:b:ds)
				x `Union` y -> processNode col nestStack (x:ds) `Union` processNode col nestStack (y:ds)
		processNode col nestStack [] = Empty
		indent col nestStack = if(spacesNeeded > 0) 
		                               then Text (replicate spacesNeeded ' ')
		                               else Empty
			where spacesNeeded = (head nestStack) - col

testnest_before = Char '{' <> Text "hello " <> Line <> Text "World " <> Text "How in the hell are you " <> Char '{' <> Text "I'm just " <> Text " fine, thank you very " <> Line <> Text "much. " <> Char '}' <> Text "Why do you " <> Line <> Text "ask?" <> Char '}'

nestedTest = nest2 3 testnest_before








         

