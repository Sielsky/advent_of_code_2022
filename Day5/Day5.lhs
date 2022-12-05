>module Day5 where
>import System.Environment 
>import Data.Char
>import Data.List
>import Text.Read

Unfortunately due to some of my courseworks deadlines I don;t have time to write explanations

There is two input files as I decided to seperate original input into two inputs which are: allignment of crates and instructions

>solve :: String -> String-> IO()
>solve filename filename2 = do
>                       crates <- readFile filename
>                       instructions <-readFile filename2
>                       putStr ("Result of all operations done by first crane ")
>                       print(fmap head $ processOperationFirstCrane (convertInstructions instructions) (processCrates crates))
>                       putStr ("Result of all operations done by second crane ")
>                       print(fmap head $ processOperationSecondCrane (convertInstructions instructions) (processCrates crates))
>                       
>                       return()

>processCrates::String->[String]
>processCrates s= fmap (filter (/=' ')) $ transpose $ map convertCrates $ lines s
>
>convertCrates::String->String
>convertCrates (n1:var:n2:[]) | isAlpha var = [var]
>                       | otherwise = [' ']
>convertCrates (n1:var:n2:xs) | isAlpha var = var:convertCrates (tail xs)
>                       | otherwise = ' ':convertCrates (tail xs)
>convertCrates []=[]
>convertCrates _ = "error"
>
>convertInstructions::String->[Int]
>convertInstructions s=filter (/=(-1)) $ fmap (convertInt . readMaybe) (words s)

This seems unnecessary but I can not find any other way to do it, 
there should be no Nothing left in the list so the nothing case is unnecessary,
but it just has to be there

>convertInt::Maybe Int->Int
>convertInt x = case x of
>                   Just n -> n
>                   Nothing -> -1

All of the bellow functions do not have patter matching exhausted but it is not necessary as we do not have to
handle errors and we have perfect input, if there would be errors possible we would have to pattern match all 
other cases to indicate errors

>processOperationFirstCrane::[Int]->[String]->[String]
>processOperationFirstCrane (x:y:z:xs) ss =processOperationFirstCrane xs (paste (cut ss y x) z (copy ss y x)) 
>processOperationFirstCrane [] ss = ss

>processOperationSecondCrane::[Int]->[String]->[String]
>processOperationSecondCrane (x:y:z:xs) ss =processOperationSecondCrane xs (pasteNoReverse (cut ss y x) z (copy ss y x)) 
>processOperationSecondCrane [] ss = ss

>copy::[String]->Int->Int->String
>copy (s:xs) n amm 
>                   |n==1 = take amm s
>                   |otherwise = copy xs (n-1) amm 

>cut::[String]->Int->Int->[String]
>cut (s:xs) n amm 
>                   |n==1 = drop amm s:xs
>                   |otherwise = s:cut xs (n-1) amm 

pasting with reversing

>paste::[String]->Int->String->[String]
>paste (s:xs) n str 
>                   |n==1 =  ((reverse str)++s):xs
>                   |otherwise = s:paste xs (n-1) str 

pasting with no reversing

>pasteNoReverse::[String]->Int->String->[String]
>pasteNoReverse (s:xs) n str 
>                   |n==1 =  (str++s):xs
>                   |otherwise = s:pasteNoReverse xs (n-1) str 

