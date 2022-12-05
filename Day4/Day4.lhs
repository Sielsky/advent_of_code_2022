>module Day4 where
>import System.Environment 
>import Data.Char
>import Data.List
>
>solve :: String -> IO()
>solve filename = do
>                       contents <- readFile filename
>                       putStr "Ammount of fully overlapping sectors "
>                       print((countTotalOverlaps.stringToSectors) contents)
>                       putStr "Ammount of partialy overlapping sectors "
>                       print((countPartialOverlaps.stringToSectors) contents)

Converts input string into list of strings each representing one pair, then further spliting each string into 4 - each representing one int,
converts those string to actual integers and concatenates it all together

>stringToSectors::String->[Int]
>stringToSectors str= map read $ concatMap (lines . splitHelper) $ lines str

helps to split string into 4 by replacing characters used to divide sections such as '-' and ',' with '\n' that is used by 
lines function to divide string into four strings

>splitHelper::String->String
>splitHelper (x:xs) 
>                   | x == '-' || x == ',' = '\n':splitHelper xs
>                   | otherwise = x:splitHelper xs
>splitHelper [] = []

since we know that the list of integers is of a form (lower boundary 1),(upper boundary 1),(lower boundary 2),(upper boundary 2)...
we can use that fact to divide the list of boundaries into fours and check if those fours contain full or partial intersections 
as I did in two functions bellow, then if the sections overlap we add 1 to the 
count of the overlaping sections to get the total ammount of them

>countTotalOverlaps:: [Int]->Int
>countTotalOverlaps (br1:ur1:br2:ur2:rs) 
>                               | br1<=br2 && ur1>=ur2 = 1+countTotalOverlaps rs
>                               | br1>=br2 && ur1<=ur2 = 1+countTotalOverlaps rs
>                               | otherwise = 0+countTotalOverlaps rs
>countTotalOverlaps [] = 0
>countTotalOverlaps _ = -1

>countPartialOverlaps:: [Int]->Int
>countPartialOverlaps (br1:ur1:br2:ur2:rs) 
>                               | br1<=br2 && br2<=ur1 = 1+countPartialOverlaps rs
>                               | br1<=ur2 && ur2<=ur1 = 1+countPartialOverlaps rs
>                               | br1<=br2 && ur1>=ur2 = 1+countPartialOverlaps rs
>                               | br1>=br2 && ur1<=ur2 = 1+countPartialOverlaps rs
>                               | otherwise = 0+countPartialOverlaps rs
>countPartialOverlaps [] = 0
>countPartialOverlaps _ = -1
