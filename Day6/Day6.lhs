>module Day6 where
>import System.Environment 
>import Data.Char
>import Data.List
>
>solve :: String -> IO()
>solve filename = do
>                       contents <- readFile filename
>                       putStr "Beggining of first tag is at "
>                       print(detectMarker(contents , 0) 4)
>                       putStr "Beggining of first message tag is at "
>                       print(detectMarker(contents , 0) 14)
>                       return()

Quite simple task today, we need to find n charactes that do not repat in a row

>detectMarker::(String,Int)->Int->(String,Int)
>detectMarker (s,x) len | length(nub(take len s))<len =detectMarker(tail s, x+1) len
>                       | length(nub(take len s))==len = (nub(take len s),x+len)
>                       | otherwise = ("fail",-1)