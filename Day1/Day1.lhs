>module Day1 where
>import System.Environment
>import Text.Read (readMaybe)
>import Data.List (sortBy)
>solve :: String -> IO()
>solve filename = do
>                       contents <- readFile filename
>                       let elfSum = listToSum (stringToList contents)
>                       putStr "The elf that caries the most has "
>                       print (show (maximum elfSum))
>                       putStr "The three elfs that carry the most have "
>                       print (show (threeTopSum elfSum))
>
>
>stringToList::String->[Maybe Int]
>stringToList x = fmap readMaybe (lines x)
>
>listToSum::[Maybe Int]->[Int]
>listToSum (Just x:Just y:xs)= listToSum (Just (x+y):xs)
>listToSum (Just x:Nothing:xs)= x:listToSum xs
>listToSum (Just x:[]) = [x]
>listToSum _ = []
>
>threeTopSum::[Int]->Int
>threeTopSum xs = sum (take 3 (sortBy (flip compare) xs))


Since I'm silly sometimes, I initialy thought I need to find out which elf is carrying the most, hence
I made the bellow function, if the return in solve is replaced by
return (findMax elfSum (maximum elfSum) 1)
we can find out which elf it was


>findMax::[Int] -> Int -> Int ->Int
>findMax (x:xs) srch acc = if x == srch then acc else findMax xs srch (acc+1) 
>findMax _ _ _= -1