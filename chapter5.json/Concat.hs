-- Why on earth did they put this into its own file???? OH, I SEE! They are not going to use it. The book merely mentions it as an example.
concat :: [[a]] -> [a]
concat = foldr (++) []