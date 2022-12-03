>module Day3 where
>import System.Environment 
>import Data.Char
>import Data.List
>
>solve :: String -> IO()
>solve filename = do
>                       contents <- readFile filename
>                       putStr "Sum of double packet items priority is "
>                       print  (sum $ fmap (convertToValue . findCommonItem. split) $ lines contents)
>                       putStr "Sum of badges priorities is "
>                       print ( sum $ fmap convertToValue $ searchForBadge $ lines contents)

Splits the string in half

>split::String->(String,String)
>split str= splitAt (div (length str) 2) str

Finds common item by using intersect function 
Note - Intersect will result in a string made of characters that are in both strings, however
since in this case we know there can only be one letter common for both string it is safe to
simply just take head of that string 

>findCommonItem::(String,String)->Char
>findCommonItem (str1, str2)= head (intersect str1 str2)

We take advantage of the fact that there is already a system that gives numbers to characters, then since ASCII characters
have slightly shifted values, we need to shift the result back

>convertToValue :: Char -> Int
>convertToValue c  
>                 | (ord c >= 65 && ord c <= 90) = (ord c) - 38
>                 | (ord c >= 97 && ord c <= 122) = (ord c) - 96
>                 | otherwise = -1

take first 3 strings from the list and apply findBadge, continue untill all strings were used

>searchForBadge::[String]->[Char]
>searchForBadge (x:y:z:xs) = (findBadge x y z) : searchForBadge xs
>searchForBadge [] = []
>searchForBadge _ = ['#']

Find common item for 3 strings, somtimes it can be two or more of the same item, that is
why we need to use head

>findBadge :: String -> String -> String -> Char
>findBadge s1 s2 s3 = head $ intersect (intersect s1 s2) s3