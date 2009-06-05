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
