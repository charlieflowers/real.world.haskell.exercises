import Data.List

main = do
	line <- getLine
	
	-- AH! For some reason, inside a do, if you want to assign something, you gotta use LET!
	let x = 3
	
	let prefix = if "shit" `isInfixOf` line
	         then "Dont say shit!"
	         else "Thanks for not saying shit!"
	
	if null line
		then return ()
		else do
			putStrLn $ (++) prefix $ reverseWords line
			-- putStrLn $ reverseWords line
			main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words 