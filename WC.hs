-- file: ch01 of real world haslekk 
-- this is the very first example in the book

main = interact wordCount
	where wordCount input = show (length (lines input)) ++ "\n"
