-- file: ch04/InteractWith.hs
-- Save this in a source file, e.g. Interact.hs

import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = getFirstWordOfEachLine

getFirstWordOfEachLine :: String -> String
getFirstWordOfEachLine contents = 
	unlines (map getFirstWord lineList)
		where 
			lineList = lines contents

getFirstWord :: String -> String
getFirstWord line = 
	case (words line) of
       []       -> []
       x:_      -> x
			