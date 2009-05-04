-- file: ch03/BookStore.hs
data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

type CustomerID = Int
type ReviewBody = String

data BookReview = BookReview BookInfo CustomerID String
				deriving (Show)

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

-- --------------------

data List a = Cons a (List a)
				| Nil
				deriving (Show)
				
fromList (x:xs) = Cons x (fromList xs)

fromList [] = Nil

-- I my SELF wrote the following ones!

toList (Cons item list) = item : toList(list)
toList (Nil) = []

data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
	deriving (Show)

mySecond :: [a] -> a
 
mySecond xs = if null (tail xs)
				then error "list too short"
				else head (tail xs)

safeSecond :: [a] -> Maybe a

safeSecond xs = if null(tail xs)
				then Nothing
				else Just (head (tail xs))
				
tidySecond :: [a] -> Maybe a

tidySecond (_:x:_) = Just x
tidySecond _ = Nothing


				
				