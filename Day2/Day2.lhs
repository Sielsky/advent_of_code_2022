>module Day2 where
>import System.Environment 
>import Data.Char
>
>data Figure = Rock|Paper|Scisor deriving (Eq)
>
>data Result = Win|Lose|Draw
>
>instance Ord Figure where
>   compare Rock Paper = LT
>   compare Paper Scisor = LT
>   compare Scisor Rock = LT
>   compare x y = if x==y then EQ else GT
>
>solve :: String -> IO()
>solve filename = do
>                       contents <- readFile filename
>                       putStr "The plan according to first rules would result in "
>                       print  ((calculate . conversion) contents)
>                       putStr "The plan according to new rules would result in "
>                       print  (calculateNew $ removeSpace contents)
>                       return()

This function just removes whitespaces

>removeSpace:: String->String
>removeSpace (x:xs) = if isSpace x then removeSpace xs else x:removeSpace xs
>removeSpace []=[]

This function converts letter to figure

>convertFig::Char->Figure
>convertFig 'A' = Rock 
>convertFig 'B' = Paper 
>convertFig 'C' = Scisor 
>convertFig 'X' = Rock
>convertFig 'Y' = Paper 
>convertFig 'Z' = Scisor

This one converts letter to result

>convertRes::Char->Result
>convertRes 'X' = Lose
>convertRes 'Y' = Draw
>convertRes 'Z' = Win

This converts input string into list of figures

>conversion::String->[Figure]
>conversion = fmap convertFig . removeSpace 

Maps figure to the score player can get for it

>scoreFig::Figure->Int
>scoreFig Rock = 1 
>scoreFig Paper = 2
>scoreFig Scisor = 3

Returns difference in score beetween player 2 and player 1 based on the outcome of a round (needs to be called together with fix function)

>scoreRes::Result->Int
>scoreRes Win = 7
>scoreRes Draw = 3
>scoreRes Lose = -1

Calculates the sum of all round scores for the first case by comparing players figures 

>calculate::[Figure]->Int
>calculate (x:y:xs)
>                   | y>x = 6 + scoreFig y + calculate xs
>                   | y==x = 3 + scoreFig y + calculate xs
>                   | y<x = 0 + scoreFig y + calculate xs
>calculate [] = 0

Calculates the sum of all round scores for second case by calculating player 1 score and adding a difference to it to acquire player 2 score

>calculateNew::String->Int
>calculateNew (x:y:xs)=  predict (convertFig x) (convertRes y) + calculateNew xs
>calculateNew [] = 0



>predict::Figure -> Result -> Int
>predict fig out=fix (scoreFig fig + scoreRes out)

As it can be clearly seen if we win we should get 7 more points than the opponent as win requires our symbol to be 1 larger than the opponent
there is only one exception when his symbol is worth 3 then ours needs to be worth 1, that is why we use fix to fix this case and similiar case that happens when
we loose and oponent put the lowest worth symbol

>fix:: Int->Int
>fix 10 = 7
>fix 0 = 3
>fix x = x